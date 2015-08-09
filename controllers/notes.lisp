(in-package :turtl)

(route (:get "/notes/([0-9a-f-]+)/file") (req res args)
  "Get a note's file. This works by generating a URL we can redirect the client
   to on the storage system then running the redirect."
  (alet* ((user-id (user-id req))
          (note-id (car args))
          (disable-redirect (get-var req "disable_redirect"))
          (file-url (get-note-file-url user-id note-id))
          (headers (unless (string= disable-redirect "1")
                     `(:location ,file-url))))
    (if file-url
        (send-response res :status (if disable-redirect 200 302) :headers headers :body (to-json file-url))
        (send-response res :status 404 :body "That note has no attachments."))))

(defun upload-local (user-id req res args)
  "Upload a file to the local filesystem."
  (let* ((note-id (car args))
         (persona-id (get-var req "persona"))
         (file-id note-id)
         (id (get-var req "id"))
         (file (hash ("id" id)))
         (path (get-file-path file-id))
         (total-file-size 0)
         (fd nil)
         (finish-fn (lambda ()
                      (catch-errors (res)
                        (close fd)
                        (vom:debug1 "file: close fd, sending final response to client")
                        (setf (gethash "size" file) total-file-size)
                        (remhash "upload_id" file)
                        (alet* ((file (edit-note-file user-id file-id file :remove-upload-id t))
                                (size (get-file-size-summary total-file-size)))
                               (track "file-upload" `(:shared ,(when persona-id t) :size ,size) req)
                          (send-json res file))))))
    (vom:debug1 "local: calling with-chunking")
    (when (string= (get-header (request-headers req) :expect) "100-continue")
      (send-100-continue res))
    (with-chunking req (data lastp)
      (vom:debug2 "local: got chunk: ~a ~a" (length data) lastp)
      (unless fd
        (vom:debug1 "file: opening local fd: ~a" path)
        (setf fd (open path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
      (incf total-file-size (length data))
      (write-sequence data fd)
      (when lastp
        (funcall finish-fn)))))

(defun upload-remote (user-id req res args)
  "Upload the given file data to a remote server."
  (alet* ((note-id (car args))
          (persona-id (get-var req "persona"))
          (file-id note-id)
          (id (get-var req "id"))
          (file (hash ("id" id)))
          (s3-uploader :starting)
          (buffered-chunks nil)
          (path (get-file-path file-id))
          (chunking-started nil)
          (last-chunk-sent nil)
          (total-file-size 0)
          (finish-fn (lambda ()
                       (catch-errors (res)
                         (vom:debug1 "file: sending final response to client")
                         (setf (gethash "size" file) total-file-size)
                         (remhash "upload_id" file)
                         (alet* ((file (edit-note-file user-id file-id file :remove-upload-id t))
                                 (size (get-file-size-summary total-file-size)))
                           (track "file-upload" `(:shared ,(when persona-id t) :size ,size) req)
                           (send-json res file))))))
    ;; create an uploader lambda, used to stream our file chunk by chunk to S3
    (vom:debug1 "file: starting uploader with path: ~a" path)
    (multiple-promise-bind (uploader upload-id)
        (s3-upload path)
      ;; save our file record
      (setf (gethash "upload_id" file) upload-id)
      (wait (edit-note-file user-id note-id file :skip-sync t)
        (vom:debug1 "file: saved file ~a" file))
      ;; save our uploader so the chunking brahs can use it
      (vom:debug1 "- file: uploader created: ~a" upload-id)
      (setf s3-uploader uploader)
      ;; if we haven't started getting the body yet, let the client know it's
      ;; ok to send
      (unless chunking-started
        (when (string= (get-header (request-headers req) :expect) "100-continue")
          (send-100-continue res)))
      (when last-chunk-sent
        (alet* ((body (flexi-streams:get-output-stream-sequence buffered-chunks))
                (finishedp (funcall s3-uploader body)))
          (incf total-file-size (length body))   ; track the file size
          ;; note that finishedp should ALWAYS be true here, but "should" and
          ;; "will" are very different things (especially in async, i'm
          ;; finding)
          (when finishedp
            (funcall finish-fn)))))
    ;; listen for chunked data. if we have an uploader object, send in our
    ;; data directly, otherwise buffer it until the uploader becomes
    ;; available
    (with-chunking req (chunk-data last-chunk-p)
      ;; notify the upload creator that chunking has started. this prevents it
      ;; from sending a 100 Continue header if the flow has already started.
      (setf chunking-started t
            last-chunk-sent (or last-chunk-sent last-chunk-p))
      (cond ((eq s3-uploader :starting)
             (unless buffered-chunks
               (vom:debug1 "- file: uploader not ready, buffering chunks")
               (setf buffered-chunks (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
             (write-sequence chunk-data buffered-chunks))
            (t
             (when buffered-chunks
               (write-sequence chunk-data buffered-chunks)
               (setf chunk-data (flexi-streams:get-output-stream-sequence buffered-chunks)))
             (incf total-file-size (length chunk-data))   ; track the file size
             (alet ((finishedp (funcall s3-uploader chunk-data (not last-chunk-p))))
               (when finishedp
                 (funcall finish-fn)))
             (setf buffered-chunks nil))))))

(route (:put "/notes/([0-9a-f-]+)/file" :chunk t :suppress-100 t) (req res args)
  "Attach file contents to a note. The HTTP content body must be the raw,
   unencoded (encrypted) file data."
  (alet* ((user-id (user-id req))
          (note-id (car args))
          (note-data (get-note-by-id note-id))
          (board-perms (get-user-board-perms user-id :min-perms 2))
          (allowed (user-can-edit-note-p user-id note-data board-perms)))
    (if allowed
        (if *local-upload*
            (upload-local user-id req res args)
            (upload-remote user-id req res args))
        (error (make-instance 'insufficient-privileges
                              :msg "Sorry, you are accessing a note you don't have access to.")))))


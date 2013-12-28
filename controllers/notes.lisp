(in-package :turtl)

;;; Many of the routes in this controller look for a persona ID passed with the
;;; request, and if found use that insteda of the currently logged-in user when
;;; updating note data. The purpose of this is to use a persona's permissions to
;;; validate changing note data intstead of the user's (in the case that the
;;; note is in a board the persona has share access to).
;;;
;;; If a persona ID is apssed, it is *always* used. The client must make the
;;; decision about whether or not to pass it, the server is not going to take
;;; both and use the one with the highest permissions.

;; TODO: just POST /notes instead of including the board id...
(defroute (:post "/api/boards/([0-9a-f-]+)/notes") (req res args)
  "Add a note. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (post-var req "persona"))
            (board-id (car args))
            (note-data (post-var req "data"))
            (note (if persona-id
                      (with-valid-persona (persona-id user-id)
                        (add-note user-id board-id note-data :persona-id persona-id))
                      (add-note user-id board-id note-data))))
      (track "note-add" `(:shared ,(when persona-id t)))
      (send-json res note))))

(defroute (:put "/api/notes/([0-9a-f-]+)") (req res args)
  "Edit a note. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (note-data (post-var req "data"))
            (note (if persona-id
                      (with-valid-persona (persona-id user-id)
                        (edit-note persona-id note-id note-data))
                      (edit-note user-id note-id note-data))))
      (track "note-edit" `(:shared ,(when persona-id t)))
      (send-json res note))))

(defroute (:delete "/api/notes/([0-9a-f-]+)") (req res args)
  "Delete a note. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (sync-ids (if persona-id
                          (with-valid-persona (persona-id user-id)
                            (delete-note persona-id note-id))
                          (delete-note user-id note-id))))
      (track "note-delete" `(:shared ,(when persona-id t)))
      (let ((hash (make-hash-table :test #'equal)))
        (setf (gethash "sync_ids" hash) sync-ids)
        (send-json res hash)))))

(defroute (:put "/api/notes/([0-9a-f-]+)/file" :chunk t :suppress-100 t :buffer-body t) (req res args)
  "Attach file contents to a note. The HTTP content body must be the raw,
   unencoded (encrypted) file data."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (note-id (car args))
            (file-id note-id)
            (hash (get-var req "hash"))
            (file (make-note-file :hash hash))
            (s3-uploader :starting)
            (buffered-chunks nil)
            (path (format nil "/files/~a" file-id))
            (chunking-started nil)
            (last-chunk-sent nil)
            (total-file-size 0)
            (finish-fn (lambda ()
                         (catch-errors (res)
                           (log:debug "file: sending final response to client")
                           (setf (gethash "size" file) total-file-size)
                           (remhash "upload_id" file)
                           (alet* ((file (edit-note-file user-id file-id file)))
                             (send-json res file))))))
      ;; create an uploader lambda, used to stream our file chunk by chunk to S3
      (log:debug "file: starting uploader with path: ~a" path)
      (multiple-future-bind (uploader upload-id)
          (s3-upload path)
        ;; save our file record
        (setf (gethash "upload_id" file) upload-id)
        (wait-for (edit-note-file user-id note-id file)
          (log:debug "file: saved file ~a" file))
        ;; save our uploader so the chunking brahs can use it
        (log:debug "- file: uploader created: ~a" upload-id)
        (setf s3-uploader uploader)
        ;; if we haven't started getting the body yet, let the client know it's
        ;; ok to send
        (unless chunking-started
          (send-100-continue res))
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
                 (log:debug "- file: uploader not ready, buffering chunks")
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
               (setf buffered-chunks nil)))))))

(defroute (:delete "/api/notes/([0-9a-f-]+)/file") (req res args)
  "Remove a note's file attachment."
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (nil (if persona-id
                     (with-valid-persona (persona-id user-id)
                       (delete-note-file persona-id note-id))
                     (delete-note-file user-id note-id))))
      (track "file-delete" `(:shared ,(when persona-id t)))
      (send-json res t))))

(defroute (:put "/api/notes/batch") (req res)
  "Batch edit. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (post-var req "persona"))
            (batch-edit-data (post-var req "data"))
            (nil (if persona-id
                     (with-valid-persona (persona-id user-id)
                       (batch-note-edit persona-id batch-edit-data))
                     (batch-note-edit user-id batch-edit-data))))
      (send-json res t))))


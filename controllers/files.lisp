(in-package :turtl)

(defroute (:post "/api/filez" :chunk t :suppress-100 t :buffer-body t) (req res)
  (catch-errors (res)
    (let ((bytes (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
          (chunk-num 0))
      (when (string= (getf (request-headers req) :transfer-encoding) "chunked")
        (send-100-continue res))
      (with-chunking req (data lastp)
        (format t "- chunk: ~a~%---~%" (babel:octets-to-string data :encoding :utf-8))
        (write-sequence data bytes)
        (incf chunk-num)
        (when lastp
          (format t "done.~%")
          (send-json res (flex:get-output-stream-sequence bytes)))))))

(defroute (:post "/api/files" :chunk t :suppress-100 t :buffer-body t) (req res)
  (catch-errors (res)
    (let* ((s3-uploader :starting)
           (user-id (user-id req))
           (file-id (get-var req "file_id"))
           (hash (get-var req "hash"))
           (buffered-chunks nil)
           (file (make-file :id file-id :hash hash :uploading t))
           (path (format nil "/files/~a" (gethash "id" file)))
           (chunking-started nil)
           (last-chunk-sent nil)
           (total-file-size 0)
           (finish-fn (lambda ()
                        (format t "- file: sending final response to client~%")
                        (setf (gethash "size" file) total-file-size)
                        (edit-file user-id file-id file)
                        (send-json res file))))
      ;; create an uploader lambda, used to stream our file chunk by chunk to S3
      (format t "- file: starting uploader with path: ~a~%" path)
      (multiple-future-bind (uploader upload-id)
          (s3-upload path)
        ;; save our file record
        (setf (gethash "upload_id" file) upload-id)
        (wait-for (edit-file user-id file-id file)
          (format t "- file: file saved: ~a~%" (gethash "id" file)))
        ;; save our uploader so the chunking brahs can use it
        (format t "- file: uploader created: ~a~%" upload-id)
        (setf s3-uploader uploader)
        ;; if we haven't started getting the body yet, let the client know it's
        ;; ok to send
        (unless chunking-started
          (send-100-continue res))
        (when last-chunk-sent
          (incf total-file-size (stream-length buffered-chunks))   ; track the file size
          (alet ((finishedp (funcall s3-uploader (flexi-streams:get-output-stream-sequence buffered-chunks))))
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
                 (format t "- file: uploader not ready, buffering chunks~%")
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


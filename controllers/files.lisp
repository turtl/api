(in-package :turtl)

(defroute (:post "/api/filez" :chunk t :suppress-100 t) (req res)
  (catch-errors (res)
    (let ((bytes (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
          ;(file (open "c:/tmp/haarwal.2.jpg"
          ;            :direction :output
          ;            :if-exists :supersede
          ;            :if-does-not-exist :create
          ;            :element-type '(unsigned-byte 8)))
          (chunk-num 0))
      (send-100-continue res)
      (with-chunking req (data lastp)
        (write-sequence data bytes)
        ;(write-sequence data file)
        (let ((sample (subseq data 0 (min 3 (length data)))))
          (format t "chunk (~a): ~a ~a (~s)~%" chunk-num (length data) (stream-length bytes) sample))
        (incf chunk-num)
        (sleep .001)
        (when lastp
          ;(close file)
          (format t "done.~%")
          (send-response res :body "thxLOL"))))))

(defroute (:post "/api/files" :chunk t :suppress-100 t) (req res)
  (catch-errors (res)
    (let* ((s3-uploader :starting)
           (user-id (user-id req))
           (buffered-chunks nil)
           (file (make-file :uploading t))
           (path (format nil "/files/~a/~a" user-id (gethash "id" file)))
           (chunking-started nil)
           (last-chunk-sent nil)
           (finish-fn (lambda ()
                        (format t "- file: sending final response to client~%")
                        (send-json res file))))
      ;; create an uploader lambda, used to stream our file chunk by chunk to S3
      (format t "- file: starting uploader with path: ~a~%" path)
      (multiple-future-bind (uploader upload-id)
          (s3-upload path)
        ;; save our file record
        (setf (gethash "upload_id" file) upload-id)
        (wait-for (save-file user-id file)
          (format t "- file: file saved: ~a~%" (gethash "id" file)))
        ;; save our uploader so the chunking brahs can use it
        (format t "- file: uploader created: ~a~%" upload-id)
        (setf s3-uploader uploader)
        ;; if we haven't started getting the body yet, let the client know it's
        ;; ok to send
        (unless chunking-started
          (send-100-continue res))
        (when last-chunk-sent
          (format t "- file: last chunk sent before upload init. sending.~%")
          (wait-for (funcall s3-uploader (flexi-streams:get-output-stream-sequence buffered-chunks))
            (funcall finish-fn))))
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
                 (format t "- file: reset chunks~%")
                 (setf buffered-chunks (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
               (write-sequence chunk-data buffered-chunks)
               ;(format t "- file: buffering data until uploader ready: ~a~%" (stream-length buffered-chunks))
               )
              (t
               (when buffered-chunks
                 (write-sequence chunk-data buffered-chunks)
                 (setf chunk-data (flexi-streams:get-output-stream-sequence buffered-chunks)))
               ;(format t "- file: uploader ready, passing in data: ~a~%" (length chunk-data))
               (wait-for (funcall s3-uploader chunk-data (not last-chunk-p))
                 (when last-chunk-p
                   (funcall finish-fn)))
               (setf buffered-chunks nil)))))))


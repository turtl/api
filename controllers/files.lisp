(in-package :turtl)

(defroute (:post "/api/files" :chunk t) (req res)
  (catch-errors (res)
    (let ((s3-upload nil)
          (saved (make-array 0 :element-type '(unsigned-byte 8)))
          (file (make-file)))
      (with-chunking req (chunk-data last-chunk-p)
        (unless s3-upload
          (setf s3-upload :starting)
          (let ((path (format nil "/files/~a" (gethash "id" file))))
            (alet ((uploader (s3-upload path :content-type "application/octet-stream")))
              (setf s3-upload uploader))))
        (if (eq s3-upload :starting)
            (setf saved (cl-async-util:append-array saved chunk-data))
            (let ((data (if saved
                            (prog1
                              (cl-async-util:append-array saved data)
                              (setf saved nil))
                            data)))
              (funcall s3-upload data (not last-chunk-p))))))))


(in-package :turtl)

(defroute (:post "/api/files" :chunk t) (req res)
  (catch-errors (res)
    (let ((s3-streamer nil)
          (file (make-file)))
      (with-chunking req (chunk-data last-chunk-p)
        (unless s3-streamer
          (setf s3-streamer (make-s3-streamer)))
        (funcall s3-streamer chunk-data)
        (when last-chunk-p
          (wait-for (funcall s3-streamer :done)

          
                                             


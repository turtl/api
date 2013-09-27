(in-package :turtl)

(defun make-s3-streamer ()
  )

(defun make-file ()
  (let ((filedata (make-hash-table :test #'equal)))
    (add-id filedata)
    filedata))


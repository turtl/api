(in-package :turtl)

(defun make-s3-streamer ()
  (let ((date (local-time:format-timestring
                nil
                (local-time:now)
                :format local-time:+rfc-1123-format+))
        (streamer (drakma-async:http-request
                    "s3.amazonaws.com"

(defun make-file ()
  "Make a file stub."
  (let ((filedata (make-hash-table :test #'equal)))
    (add-id filedata)
    filedata))


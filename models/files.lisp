(in-package :turtl)

(defun make-file ()
  "Make a file stub."
  (let ((filedata (make-hash-table :test #'equal)))
    (add-id filedata)
    filedata))


(in-package :turtl)

(defun make-file (&key uploading)
  "Make a file stub."
  (let ((filedata (make-hash-table :test #'equal)))
    (add-id filedata)
    (when uploading (setf (gethash "status" filedata) "u"))
    filedata))

(defafun save-file (future) (user-id filedata)
  "Saves a file record. Does *not* contain file contents, which will be streamed
   separately to storage system."
  (setf (gethash "user_id" filedata) user-id)
  (add-mod filedata)
  (alet* ((sock (db-sock))
          (query (r:r (:insert
                        (:table "files")
                        filedata)))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future filedata)))


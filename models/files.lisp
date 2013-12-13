(in-package :turtl)

(defvalidator validate-file
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("hash" :type string :required t)
   ("upload_id" :type string)
   ("body" :type cl-async-util:bytes-or-string)))

(defun make-file (&key id hash)
  "Make a file stub."
  (let ((filedata (make-hash-table :test #'equal)))
    (if id
        (setf (gethash "id" filedata) id)
        (add-id filedata))
    (when hash (setf (gethash "hash" filedata) hash))
    filedata))

(defafun get-user-file-permissions (future) (user-id file-id)
  "'Returns' an integer used to determine a user's permissions for the given
   file.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((sock (db-sock))
          (query (r:r (:== (:attr (:get (:table "files") file-id) "user_id") user-id)))
          (file-owner-p (r:run sock query)))
    (r:disconnect sock)
    (finish future (if file-owner-p
                       3
                       0))))

(defafun add-file (future) (user-id filedata)
  "Adds a file record. Does *not* contain file contents, which will be streamed
   separately to storage system."
  (setf (gethash "user_id" filedata) user-id)
  (alet* ((sock (db-sock))
          (query (r:r (:insert
                        (:table "files")
                        filedata)))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future filedata)))

(defafun edit-file (future) (user-id file-id filedata)
  "Edits a file record. Does *not* contain file contents, which will be streamed
   separately to storage system."
  (alet ((perms (get-user-file-permissions user-id file-id)))
    (if (<= 2 perms)
        (progn
          ;; disallow ownership change
          (remhash "user_id" filedata)
          (alet* ((sock (db-sock))
                  (file-id (gethash "id" filedata))
                  ;; NOTE: we replace instead of edit since in many cases, we
                  ;; want to remove certain keys (like upload_id)
                  (query (r:r (:replace
                                (:get (:table "files") file-id)
                                filedata)))
                  (nil (r:run sock query)))
            (r:disconnect sock)
            (finish future filedata)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a file you don't have access to.")))))

(defafun file-delete (future) (user-id filedata)
  "Delete a file record/data. Named file-delete instead of delete-file because
   the latter is already defined by the CL spec and I'd sooner give up
   consistency than have to shadow the symbol and people reading the code be
   like 'omg lol wtf'."
  (finish future nil))


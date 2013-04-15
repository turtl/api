(in-package :tagit)

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun load-folder (path)
  "Load all lisp files in a directory."
  (dolist (file (directory (concatenate 'string path "*.lisp")))
    (load file)))

(defun db-sock ()
  "Makes connecting to the database a smidgen easier."
  (r:connect *db-host* *db-port* :db *db-name* :read-timeout 2))

(defun send-json (response object)
  "Wraps sending of JSON back to the client."
  (send-response response
                 :headers '(:content-type "application/json")
                 :body (with-output-to-string (s) (yason:encode object s))))

(defmacro defvalidator (name &body body)
  "Makes defining a validation function for a data type simpler."
  (let ((object (gensym "object")))
    `(defun ,name (,object)
       (declare (ignore ,object))
       ,@body
       nil)))

(defun add-id (hash-object &key (id "id"))
  "Add a mongo id to a hash table object."
  (setf (gethash id hash-object) (cl-mongo-id:oid)))

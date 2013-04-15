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

(defun do-validate (object validation-form &key edit)
  "Validation a hash object against a set of rules. Returns nil on *success* and
   returns the errors on failure."
  nil)

(defmacro defvalidator (name validation-form)
  "Makes defining a validation function for a data type simpler."
  `(defmacro ,name ((object future &key edit) &body body)
     (let ((validation (gensym "validation")))
       `(let ((,validation (do-validate ,object ,'',validation-form :edit ,edit)))
          (if ,validation
              (signal-error ,future (make-instance 'validation-failed
                                                   :msg (format nil "Validation failed: ~s~%" ,validation)))
              (progn ,@body))))))

(defun add-id (hash-object &key (id "id"))
  "Add a mongo id to a hash table object."
  (setf (gethash id hash-object) (string-downcase (mongoid:oid-str (mongoid:oid)))))


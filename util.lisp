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

(defun to-json (object)
  "Convert an object to JSON."
  (with-output-to-string (s)
    (yason:encode object s)))

(defun send-json (response object)
  "Wraps sending of JSON back to the client."
  (send-response response
                 :headers '(:content-type "application/json")
                 :body (to-json object)))

(defun add-id (hash-object &key (id "id"))
  "Add a mongo id to a hash table object."
  (setf (gethash id hash-object) (string-downcase (mongoid:oid-str (mongoid:oid)))))

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

(defmacro defafun (name (future-var &key (forward-errors t)) args &body body)
  "Define an asynchronous function with a returned future that will be finished
   when the funciton completes. Also has the option to forward all async errors
   encountered during excution (in this lexical scope) to the returned future."
  (let* ((docstring (car body))
         (docstring (if (stringp docstring)
                        docstring
                        nil)))
    (when (stringp docstring)
      (setf body (cdr body)))
    `(defun ,name ,args
       ,docstring
       (let ((,future-var (make-future)))
         ,(if forward-errors
              `(future-handler-case
                 (progn ,@body)
                 ((or error simple-error) (e)
                  (signal-error ,future-var e)))
              `(progn ,@body))
         ,future-var))))

(defun error-json (err)
  "Convert an error object to JSON."
  (let ((msg (error-msg err)))
    (to-json msg)))

(defmacro catch-errors ((response) &body body)
  "Define a macro that catches errors and responds via HTTP to them."
  `(future-handler-case
     (progn ,@body)
     ;; catch errors that can be easily transformed to HTTP
     (tagit-error (e)
      (send-response ,response
                     :status (error-code e)
                     :headers '(:content-type "application/json")
                     :body (error-json e)))
     ((or cl-rethinkdb:query-error cl-rethinkdb:cursor-error) (e)
      (send-response ,response
                     :status 500
                     :headers '(:content-type "application/json")
                     :body (to-json
                             (with-output-to-string (s)
                               (format s "Internal server error. Please report to ~a" *admin-email*)
                               (when *display-errors*
                                 (format s "~%(~a)" (type-of e))
                                 (if (typep e 'cl-rethinkdb:query-error)
                                     (format s ": ~a~%" (cl-rethinkdb::query-error-msg e))
                                     (format s "~%")))))))))
                     
       


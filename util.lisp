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

(defun send-json (response object &key (status 200))
  "Wraps sending of JSON back to the client."
  (send-response response
                 :status status
                 :headers '(:content-type "application/json")
                 :body (to-json object)))

(defun add-id (hash-object &key (id-key "id"))
  "Add a mongo id to a hash table object."
  (setf (gethash id-key hash-object) (string-downcase (mongoid:oid-str (mongoid:oid)))))

(defun add-mod (hash-object &key (key "mod"))
  "Add a mongo id to a hash table object."
  (setf (gethash key hash-object) (get-timestamp)))

(defun get-timestamp ()
  "Get the current unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))

(defun parse-float (string)
  "Return a float read from string, and the index to the remainder of string."
  (multiple-value-bind (integer i)
      (parse-integer string :junk-allowed t)
    (when (<= (1- (length string)) i) (return-from parse-float integer))
    (multiple-value-bind (fraction j)
        (parse-integer string :start (+ i 1) :junk-allowed t)
      (values (float (+ integer (/ fraction (expt 10 (- j i 1))))) j))))

(defun sha256 (sequence/string)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (string-downcase
    (to-hex
      (ironclad:digest-sequence (ironclad:make-digest 'ironclad:sha256)
                                (if (stringp sequence/string)
                                    (babel:string-to-octets sequence/string)
                                    sequence/string)))))

(defun crypto-random ()
  "Generate a cryptographically-secure random number between 0 and 1. Works by
   calling out to OpenSSL."
  (coerce (/ (secure-random:number most-positive-fixnum) most-positive-fixnum) 'single-float))

(defun to-hex (byte-array)
  "Covert a byte array to a hex string."
  (let ((hex-string (make-string (* (length byte-array) 2))))
    (loop for byte across byte-array
          for i from 0 by 2 do
      (let ((byte-hex (format nil "~2,'0X" byte)))
        (setf (aref hex-string i) (aref byte-hex 0)
              (aref hex-string (1+ i)) (aref byte-hex 1))))
    hex-string))

(defmacro varint (str-input &optional default)
  "Wraps parsing an integer from a form where the return may be a string integer
   or may be nil. Meant to be used to parse numbers from GET/POST data."
  `(or (parse-integer (or ,str-input "") :junk-allowed t) ,default))

(defun do-validate (object validation-form &key edit)
  "Validation a hash object against a set of rules. Returns nil on *success* and
   returns the errors on failure."
  (flet ((val-form (key)
           (let ((form nil))
             (dolist (entry validation-form)
               (when (string= key (car entry))
                 (setf form entry)
                 (return)))
             form))
         (val-error (str)
           (return-from do-validate str)))
    (dolist (entry validation-form)
      (block do-validate
        (let* ((key (car entry))
               (entry (cdr entry))
               (entry-type (getf entry :type))
               (obj-entry (multiple-value-list (gethash key object)))
               (obj-val (car obj-entry))
               (exists (cadr obj-entry))
               (default-val (getf entry :default)))
          ;; check required fields
          (when (and (getf entry :required)
                     (not edit)
                     (not obj-val))
            (cond ((and default-val (symbolp default-val))
                   (setf obj-val (funcall default-val)))
                  (default-val
                   (setf obj-val default-val))
                  (t
                   (val-error (format nil "Required field `~a` not present." key)))))

          ;; if the field doesn't exist, there's no point in validating it further
          (unless exists (return-from do-validate))

          ;; do some typing work
          (when entry-type
            ;; convert strings to int/float if needed
            (when (and (typep obj-val 'string)
                       (subtypep entry-type 'number))
              (let ((new-val (ignore-errors (parse-float obj-val))))
                (when new-val
                  (setf obj-val new-val))))
            ;; make sure the types match up
            (when (not (typep obj-val entry-type))
              (val-error (format nil "Field `~a` is not of the expected type ~a" key entry-type))))
          
          (case entry-type
            (string
              (let ((slength (getf entry :length)))
                (when (and (integerp slength)
                           (not (= slength (length obj-val))))
                  (val-error (format nil "Field `~a` is not the required length (~a characters)" key slength))))))

          ;; TODO validate subobject/subsequence

          ;; set the value (in its processed form) back into the object
          (setf (gethash key object) obj-val))))
    ;; remove junk keys from object data
    (loop for key being the hash-keys of object do
      (unless (val-form key)
        (remhash key object))))
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
  (let* ((docstring (car body)))
    (when (stringp docstring)
      (setf body (cdr body)))
    `(defun ,name ,args
       ,(if (stringp docstring) docstring "")
       (let ((,future-var (make-future)))
         ,(if forward-errors
              `(future-handler-case
                 (progn ,@body)
                 (t (e) (signal-error ,future-var e)))
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
     ;; catch anything else and send a response out for it
     (t (e)
      (format t "(tagit) Caught error: ~a~%" e)
      (if (wookie:response-finished-p ,response)
          (wookie-util:wlog :error "(tagit) ...double error: ~a~%" e)
          (unless (as:socket-closed-p (get-socket ,response))
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
                                           (format s ": ~a~%" e)))))))))))

(defun is-public-action (method path)
  "Checks if the given method/path combination are in the configured list of
   actions that do not require a user login."
  (loop for (check-method . check-resource) in *public-actions* do
    (let ((check (ignore-errors   ;; just in case
                   (and (eq check-method method)
                        (if (stringp check-resource)
                            ;; it's a bording ol' string
                            (string= check-resource path)
                            ;; it's a regex (hopefully)
                            (cl-ppcre:scan check-resource path))))))
      (when check (return-from is-public-action t)))))


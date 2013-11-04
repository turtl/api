(in-package :turtl)

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun load-folder (path &optional load-order)
  "Load all lisp files in a directory."
  (let* ((filename (lambda (x) (pathname-name (pathname x))))
         (file-list (directory (concatenate 'string path "*.lisp")))
         ;; if we have a load-order, do our best to sort the filesystem entries
         ;; by that order. if a file isn't in the order list, it will come after
         ;; all files that are in the list, and will be sorted at the end by its
         ;; name.
         (file-list (if load-order
                        (sort
                          file-list
                          (lambda (a b)
                            ;; grab the basename for the file and determine its
                            ;; position in the load-order
                            (let* ((a (funcall filename a))
                                   (b (funcall filename b))
                                   (a-pos (or (position a load-order :test #'string=) 9999))
                                   (b-pos (or (position b load-order :test #'string=) 9999))
                                   (sortval (- b-pos a-pos)))
                              (if (zerop sortval)
                                  ;; no load-order position, sort by name
                                  (string< a b)
                                  ;; return nil if sortval < 0
                                  (when (< 0 sortval) sortval)))))
                        file-list)))
    (dolist (file file-list)
      (load file))
    file-list))

(defun db-sock ()
  "Makes connecting to the database a smidgen easier."
  (r:connect *db-host* *db-port* :db *db-name* :read-timeout 15))

(defun db-index (table index)
  "Grab the correct index name from the db schema."
  (let* ((table-key (if (keywordp table)
                        table
                        (intern (string-upcase table) :keyword)))
         (index-key (if (keywordp index)
                        index
                        (intern (string-upcase index) :keyword)))
         (indexes (getf (getf *db-schema* table-key) :indexes))
         (index-entry (getf indexes index-key)))
    (unless index-entry
      (error (format nil "Bad index name passed to db-index: ~a" index)))
    (format nil "~a.v~a" (string-downcase (string index-key)) (getf index-entry :version))))

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

(defun convert-alist-hash (alist &key (test #'equal))
  "Convert an alist into a hash table. Only works on flat alists (nesting
   doesn't work)."
  (let ((hash (make-hash-table :test test)))
    (dolist (entry alist)
      (let ((key (car entry))
            (val (cdr entry)))
        (setf (gethash key hash) val)))
    hash))

(defun convert-plist-hash (plist &key (test #'equal))
  "Convert an plist into a hash table. Only works on flat plists (nesting
   doesn't work)."
  (let ((hash (make-hash-table :test test)))
    (loop for (key val) on plist by #'cddr do
      (setf (gethash (string-downcase (string key)) hash) val))
    hash))

(defun add-id (hash-object &key (id-key "id"))
  "Add a mongo id to a hash table object."
  (setf (gethash id-key hash-object) (string-downcase (mongoid:oid-str (mongoid:oid))))
  hash-object)

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
                 (t (e)
                   ;; wrap the caught error in the error wrapper, which when
                   ;; printed out gives us the name of the function the error
                   ;; occurred in. makes debugging, oh, about 1000000x easier.
                   (signal-error ,future-var
                                 (make-instance 'turtl-error-wrapper
                                                :error e
                                                :function ',name))))
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
     (turtl-error (e)
      (send-response ,response
                     :status (error-code e)
                     :headers '(:content-type "application/json")
                     :body (error-json e)))
     ;; catch anything else and send a response out for it
     (t (e)
      (format t "(turtl) Caught error: ~a~%" e)
      (if (wookie:response-finished-p ,response)
          (wookie-util:wlog :error "(turtl) ...double error: ~a~%" e)
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

(defmacro with-valid-persona ((persona-id user-id &optional future) &body body)
  "Wraps persona verification into a nice little package."
  `(aif (persona-owned-by-user-p ,persona-id ,user-id)
        (progn ,@body)
        ,(if future
             `(signal-error ,future (make-instance 'insufficient-privileges :msg "Sorry, persona verification failed."))
             `(error 'insufficient-privileges :msg "Sorry, persona verification failed."))))

(defun get-current-pid (&key if-not-exists-return)
  "Get the current process' PID. This function does it's best to be cross-
  implementation. If it isn't able to grab the PID from the system, it defaults
  to returning whatever value is passed into the :if-not-exists-return key."
  #+clisp
  (system::process-id)
  #+(and lispworks unix)
  (system::getpid)
  #+(and sbcl unix)
  (sb-unix:unix-getpid)
  #+(and cmu unix)
  (unix:unix-getpid)
  #+openmcl
  (ccl::getpid)
  #-(or clisp (and lispworks unix) (and sbcl unix) (and cmu unix) (and openmcl unix) openmcl)
  if-not-exists-return)


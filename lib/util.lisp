(in-package :turtl)

(defun user-id (request)
  "Grab a user id from a request."
  (let ((data (request-data request)))
    (when (hash-table-p data)
      (gethash "id" data))))

(defun post-body (request)
  "Grab a hash-table JSON object if the body is JSON."
  (jonathan:parse (babel:octets-to-string (request-body request)) :as :hash-table))

(defun get-client (request)
  "Grab the current client ID (desktop v0.4.1, chrome v0.5.6, etc)"
  (let ((headers (request-headers request)))
    (get-header headers :x-turtl-client)))

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defmacro route (options (req res &optional args) &body body)
  "Wrapper around wookie's defroute. Adds error handling and prepends our
   *api-path* var to the resource."
  (let* ((resource (cadr options))
         (path (if (boundp '*api-path*)
                   (symbol-value '*api-path*)
                   ""))
         (resource (concatenate 'string path resource)))
    (setf (cadr options) resource)
    `(defroute ,options (,req ,res ,args)
       ,(when (stringp (car body))
          (let ((docstr (car body)))
            (setf body (cdr body))
            docstr))
       (catch-errors (,res)
         ,@body))))

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

(defun db-sock (&key (db *db-name*) (timeout 60))
  "Makes connecting to the database a smidgen easier."
  (r:connect *db-host* *db-port* :db db :read-timeout timeout))

(defun db-index (table index &key (schema *db-schema*))
  "Grab the correct index name from the db schema."
  (let* ((table-key (if (keywordp table)
                        table
                        (intern (string-upcase table) :keyword)))
         (index-key (if (keywordp index)
                        index
                        (intern (string-upcase index) :keyword)))
         (indexes (getf (getf schema table-key) :indexes))
         (index-entry (getf indexes index-key)))
    (unless index-entry
      (error (format nil "Bad index name passed to db-index: ~a" index)))
    (format nil "~a.v~a" (string-downcase (string index-key)) (getf index-entry :version))))

(defmethod jonathan:%to-json ((_ (eql nil)))
  (jonathan:%write-string "null"))

(defmethod jonathan:%to-json ((error error))
  (jonathan:%write-string (format nil "~a~%" error)))

(defun to-json (object &key indent)
  "Convert an object to JSON."
  (declare (ignore indent))
  (jonathan:to-json object))

(defun send-json (response object &key (status 200))
  "Wraps sending of JSON back to the client."
  (let* ((request (response-request response))
         (http (request-http request))
         (sock (request-socket request)))
    (if (as:socket-closed-p sock)
        (vom:notice "sending response to closed socket (~a ~a)"
                    (request-method request)
                    (request-resource request))
        (send-response response
                       :status status
                       :headers '(:content-type "application/json")
                       :body (to-json object)))))

(defun convert-alist-hash (alist &key (test #'equal))
  "Convert an alist into a hash table. Only works on flat alists (nesting
   doesn't work)."
  (let ((hash (make-hash-table :test test)))
    (dolist (entry alist)
      (let ((key (car entry))
            (val (cdr entry)))
        (setf (gethash key hash) val)))
    hash))

(defun convert-plist-hash (plist &key (test #'equal) convert-nulls)
  "Convert an plist into a hash table. Only works on flat plists (nesting
   doesn't work)."
  (let ((hash (make-hash-table :test test)))
    (loop for (x y) on plist by #'cddr do
      (setf (gethash (string-downcase (string x)) hash) y))
    hash))

(defun copy-hash (hash)
  "Deep copy a hash table."
  ;; lazy way
  (jonathan:parse (jonathan:to-json hash) :as :hash-table))

(defun make-id ()
  "Make a new server-generated ID."
  (string-downcase (mongoid:oid-str (mongoid:oid))))

(defun add-id (hash-object &key (id-key "id"))
  "Add a mongo id to a hash table object."
  (setf (gethash id-key hash-object) (make-id))
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

(defmacro varint (str-input &optional default)
  "Wraps parsing an integer from a form where the return may be a string integer
   or may be nil. Meant to be used to parse numbers from GET/POST data."
  `(or (parse-integer (or ,str-input "") :junk-allowed t) ,default))

(defun stream-length (stream)
  "HACK: flexi-streams reports incorrect length on stream data body, so we do a
   bit of vigilantism here to fix it."
  (with-accessors ((flexi-streams::vector flexi-streams::vector-stream-vector))
      stream
    (length vector)))

(defmacro adefun (name args &body body)
  "Define a function that wraps things in some convenient error handling."
  (let* ((docstring (car body)))
    (when (stringp docstring)
      (setf body (cdr body)))
    `(defun ,name ,args
       ,(if (stringp docstring) docstring "")
       ,(when (eq (caar body) 'declare)
          (prog1 (car body)
            (setf body (cdr body))))
       (catcher
         (block ,name ,@body)
         (error (e)
           (vom:error "wrapping (~a): ~a" ',name e)
           (add-server-log e (format nil "adefun ~a" ',name))
           ;; wrap the caught error in the error wrapper, which when
           ;; printed out gives us the name of the function the error
           ;; occurred in. makes debugging, oh, about 6000x easier.
           (error (make-instance 'turtl-error-wrapper
                                 :error e
                                 :function ',name)))))))

;; !!!!!!!!!!!!!!!!!!
;; !!! DEPRECATED !!!
;; !!!!!!!!!!!!!!!!!!
;; do not build new functions that use this macro. use adefun instead!
(defmacro defafun (name (future-var &key (forward-errors t)) args &body body)
  "Define an asynchronous function with a returned promise that will be finished
   when the function completes. Also has the option to forward all async errors
   encountered during excution (in this lexical scope) to the returned promise"
  (let* ((docstring (car body)))
    (when (stringp docstring)
      (setf body (cdr body)))
    `(defun ,name ,args
       ,(if (stringp docstring) docstring "")
       (let ((,future-var (make-promise)))
         ,(if forward-errors
              `(catcher
                 (progn ,@body)
                 (error (e)
                   ;; wrap the caught error in the error wrapper, which when
                   ;; printed out gives us the name of the function the error
                   ;; occurred in. makes debugging, oh, about 6000x easier.
                   ;;
                   ;; also, log everything.
                   (vom:error "wrapping (~a): ~a" ',name e)
                   (add-server-log e (format nil "defafun: ~a" ',name))
                   (signal-error ,future-var (make-instance 'turtl-error-wrapper
                                                            :error e
                                                            :function ',name))))
              `(progn ,@body))
         ,future-var))))

(defun error-json (err)
  "Convert an error object to JSON."
  (let ((msg (error-msg err)))
    (to-json msg)))

(defun print-backtrace (err)
  "Standard function to print an error backtrace."
  (vom:error "-- Backtrace: ~a~%" (trivial-backtrace:print-backtrace err :output nil)))

(defmacro catch-errors ((response) &body body)
  "Define a macro that catches errors and responds via HTTP to them."
  `(catcher
     (progn ,@body)
     ;; catch errors that can be easily transformed to HTTP
     (turtl-error (e)
       (send-response ,response
                      :status (error-code e)
                      :headers '(:content-type "application/json")
                      :body (error-json e)))
     ;; catch anything else and send a response out for it
     (error (e)
       (vom:error "Caught error: ~a" e)
       (print-backtrace e)
       (if (wookie:response-finished-p ,response)
           (vom:error "(turtl) ...double error: ~a" e)
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

(defun mem-mon (&optional (time 3))
  "Returns an plist with two fields: start (function) and stop (function). When
   start is called, `(room)` is called every `time` seconds indefinitely (or
   until `(stop)` is called."
  (let ((enabled nil))
    (labels ((run ()
               (when enabled
                 (room)
                 (as:delay #'run :time time))))
      (list :start (lambda ()
                     (setf enabled t)
                     (run))
            :stop (lambda () (setf enabled nil))))))


(defun jprint (db-result)
  "Pretty printer for JSON (mainly for database results)."
  (to-json db-result :indent 2))

(defmacro with-test (&body body)
  "Makes testing async functions easier by abstracting an extremely common
   pattern."
  `(as:with-event-loop (:catch-app-errors t)
     (catcher
       (progn ,@body)
       (error (e) (format t "err: ~a~%" e)))))


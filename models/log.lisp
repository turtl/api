(in-package :turtl)

(defun hash-log (log-data)
  "Generate a hash for this log from its data. Used to de-dupe log records."
  (flet ((ensure-string (x)
           (if (stringp x) x (write-to-string x))))
    (md5 (concatenate
           'string
           (ensure-string (gethash "msg" log-data))
           (ensure-string (gethash "url" log-data))
           (ensure-string (gethash "line" log-data))
           (ensure-string (gethash "version" log-data))))))

(defafun add-server-log (future) (err location)
  "Add a server log entry (when we catch errors that shouldn't necessarily
   be returned to he user). This is a simple wrapper around add-log."
  (vom:error "server caught error: (~a): ~a" location err)
  (unless (typep err '(or as:streamish-eof as:socket-timeout))
    (let ((log-data (make-hash-table :test #'equal)))
      (setf (gethash "msg" log-data) (format nil "~a" err)
            (gethash "url" log-data) location
            (gethash "line" log-data) "0"
            (gethash "version" log-data) "api")
      (alet ((res (add-log log-data)))
        (finish future res)))))

(defafun add-log (future) (log-data)
  "Add a log entry to the DB."
  ;; check for empty data
  (unless log-data
    (finish future nil)
    (return-from add-log future))
  ;; basically, filter out old firefox errors here (or any client that's too old
  ;; to give us a valid client version).
  (let ((client-version (gethash "version" log-data)))
    (when (or (not client-version)
              (string= client-version ""))
      (finish future nil)
      (return-from add-log future)))
  (setf (gethash "url" log-data) (cl-ppcre:regex-replace "^.*/data/app" (gethash "url" log-data) "/data/app"))
  (catcher
    (alet* ((log-hash (hash-log log-data))
            (log-entry (let ((hash (make-hash-table :test #'equal)))
                         (setf (gethash "id" hash) log-hash
                               (gethash "data" hash) log-data
                               (gethash "created" hash) (get-timestamp)
                               (gethash "c" hash) 1)
                         hash))
            (sock (db-sock))
            (query (r:r
                     (:replace
                       (:get (:table "log") log-hash)
                       (r:fn (x)
                         (:branch
                           (:== (:default x 0) 0)
                           log-entry
                           (:merge
                             x
                             `(("c" . ,(:+ (:attr x "c") 1)))))))))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future log-entry))
    (error (e)
      (vom:error "add-log: ~a" e))))


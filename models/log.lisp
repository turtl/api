(in-package :turtl)

(defun hash-log (log-data)
  "Generate a hash for this log from its data. Used to de-dupe log records."
  (md5 (concatenate
         'string
         (gethash "msg" log-data)
         (gethash "url" log-data)
         (gethash "line" log-data)
         (gethash "version" log-data))))

(defafun add-log (future) (log-data)
  "Add a log entry to the DB."
  ;; basically, filter out old firefox errors here (or any client that's too old
  ;; to give us a value client version).
  (let ((client-version (gethash "version" log-data)))
    (when (or (not client-version)
              (string= client-version ""))
      (finish future nil)
      (return-from add-log)))
  (setf (gethash "url" log-data) (cl-ppcre:regex-replace "^.*/data/app" (gethash "url" log-data) "/data/app"))
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
          (res (r:run sock query)))
    (r:disconnect sock)
    (finish future log-entry)))


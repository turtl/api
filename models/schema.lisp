(in-package :turtl)

(defun schema-name-to-string (keyword)
  "Convert a schema enrtry into a string, ie ':user' becomes \"user\"."
  (let* ((str (string keyword))
         (str (String-downcase str))
         (str (cl-ppcre:regex-replace-all "-" str "_")))
    str))

(defafun create-turtl-db (future) (name)
  "Create the given databse, if it doesn't exist. Returns the REQL db object for
   the given db either way."
  (alet* ((sock (db-sock))
          (query (r:r (:db-list)))
          (dbs (r:run sock query)))
    (if (find name dbs :test #'string=)
        (progn 
          (r:disconnect sock)
          (finish future (r:r (:db name))))
        (alet* ((query (r:r (:db-create name)))
                (nil (r:run sock query)))
          (r:disconnect sock)
          (finish future (r:r (:db name)))))))

(defafun apply-indexes (future) (table schema)
  "Apply the given indexes to the given table."
  (alet* ((table-name (schema-name-to-string table))
          (sock (db-sock))
          (query (r:r (:index-list (:table table-name))))
          (schema-index-names (loop for (name) on schema by #'cddr collect name))
          (remove-version-fn (lambda (x) (cl-ppcre:regex-replace-all "\.v.*$" x "")))
          (indexes (r:run sock query))
          (indexes-no-version (mapcar remove-version-fn indexes)))
    ;; keep track of what we're doing so we can report back
    (let ((to-add nil)
          (to-edit nil)
          (to-delete nil))
      ;; define our helpful helper functions
      (labels ((add-index (index-key)
                 (alet* ((index-schema (getf schema index-key))
                         (base-name (string-downcase (string index-key)))
                         (index-name (format nil "~a.v~a"
                                             base-name
                                             (getf index-schema :version)))
                         (index-fn (if (getf index-schema :function)
                                       (getf index-schema :function)
                                       (r:fn (rec)
                                         (:attr rec base-name))))
                         (multi (getf index-schema :multi))
                         (query (r:r (:index-create
                                       (:table table-name)
                                       index-name
                                       :function index-fn
                                       :multi multi)))
                         (nil (r:run sock query)))
                   t))
               (delete-index (index-name)
                 (alet* ((query (r:r (:index-drop (:table table-name) index-name)))
                         (nil (r:run sock query)))
                   t))
               (edit-index (old-index-name index-key)
                 (let ((future (make-future)))
                   (wait-for (delete-index old-index-name)
                     (wait-for (add-index index-key)
                       (finish future)))
                   future)))
        ;; populate to-add list
        (dolist (index-key schema-index-names)
          (let* ((schema-index-name (schema-name-to-string index-key)))
            (unless (find schema-index-name indexes-no-version :test #'string=)
              (push index-key to-add))))
        (let ((schema-index-name-strings (mapcar (lambda (x) (schema-name-to-string x))
                                                 schema-index-names)))
          ;; populate to-delete
          (dolist (db-index indexes)
            (unless (find (funcall remove-version-fn db-index) schema-index-name-strings :test #'string=)
              (push db-index to-delete))))
          
        ;; populate to-edit
        (dolist (index-key schema-index-names)
          (let* ((index (getf schema index-key))
                 (index-version (getf index :version))
                 (existing-index-pos (position
                                       (schema-name-to-string index-key)
                                       indexes-no-version
                                       :test #'string=)))
            (when existing-index-pos
              (let* ((existing-index (nth existing-index-pos indexes))
                     (existing-version-str (cl-ppcre:regex-replace ".*\.v([0-9]+).*?$" existing-index "\\1"))
                     (existing-version (or (ignore-errors (parse-integer existing-version-str :junk-allowed t)) 0)))
                (unless (= existing-version index-version)
                  (push (cons index-key existing-index) to-edit))))))

        (wait-for (adolist (index-key to-add)
                    (add-index index-key))
          (wait-for (adolist (index-name to-delete)
                      (delete-index index-name))
            (wait-for (adolist (entry to-edit)
                        (let ((index-key (car entry))
                              (old-index-name (cdr entry)))
                          (edit-index old-index-name index-key)))
              (r:disconnect sock)
              (let ((report nil))
                (dolist (key to-add) (push (cons key :add) report))
                (dolist (name to-delete) (push (cons name :delete) report))
                (dolist (entry to-edit) (push (cons (car entry) :upgrade) report))
                (finish future report)))))))))

(defafun apply-db-schema (future) (schema)
  "Apply the given database schema to the current database. This will not only
   create tables, but also create/upgrade/remove indexes based on their version
   and their presence in the schema."
  (alet* ((db (create-turtl-db *db-name*))
          (tables-add nil)
          (indexes nil)
          (sock (db-sock))
          (query (r:r (:table-list db)))
          (tables (r:run sock query)))
    (r:disconnect sock)
    ;; track the tables we need to create (by seeing if there are schema tables
    ;; that aren't in the returned table list)
    (let ((schema-tables (loop for (name) on schema by #'cddr collect name))
          (to-add nil))
      (dolist (schema-table schema-tables)
        (let ((table-name (schema-name-to-string schema-table)))
          (unless (find table-name tables :test #'string=)
            (push table-name to-add))))
      ;; add the tables, in sequence
      (wait-for (adolist (table to-add)
                  (alet* ((sock (db-sock))
                          (query (r:r (:table-create db table)))
                          (nil (r:run sock query)))
                    (r:disconnect sock)
                    (push table tables-add)))
        ;; all tables added, now do index updates
        (wait-for (adolist (table schema-tables)
                    (alet ((status (apply-indexes table (getf (getf schema table) :indexes))))
                      (dolist (entry status)
                        (push (car entry) (getf (getf indexes table) (cdr entry))))))
          (finish future (list :tables-add tables-add
                               :indexes indexes)))))))

;; ----- test
(defun test ()
  (as:with-event-loop (:catch-app-errors t)
    (future-handler-case
      (alet ((report (apply-db-schema *db-schema*)))
        (format t "report: ~s~%" report))
      (t (e) (format t "err: ~a~%" e)))))
;; ----- /test
    

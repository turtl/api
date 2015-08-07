(ql:quickload :turtl)
(vom:config t :info)
(vom:config :cl-rethinkdb :warn)
(vom:config :turtl :debug)

(in-package :turtl)

(defun apply-schema ()
  (as:with-event-loop ()
    ;; set up the database schema
    (vom:info "Applying DB schema...")
    (catcher
      (alet* ((report-main (apply-db-schema *db-schema*))
              (report-analytics (when (getf *analytics* :enabled)
                                  (apply-db-schema *analytics-schema* :db-name (or (getf *analytics* :db) "analytics"))))
              (report (append report-main report-analytics)))
        (vom:info "Schema applied: ~s" report)))))

(apply-schema)
(cl-user::quit)


(ql:quickload '(:cl-async-future :cl-async :cl-rethinkdb :turtl))

(defpackage :upgrade-personas
  (:use :cl :cl-async-future))
(in-package :upgrade-personas)

(defparameter *db-host* "127.0.0.1")
(defparameter *db-port* 5000)

(defun run ()
  (as:with-event-loop (:catch-app-errors t)
    (future-handler-case
      (alet* ((sync-recs nil)
              (sock (r:connect *db-host* *db-port* :db "turtl" :read-timeout 15))
              (qry (r:r (:table "personas")))
              (cursor (r:run sock qry))
              (personas (r:to-array sock cursor)))
        (r:stop/disconnect sock cursor)
        (loop for persona across personas do
          (let* ((user-id (gethash "user_id" persona))
                 (persona-id (gethash "id" persona))
                 (sync-rec (turtl::make-sync-record
                             user-id
                             "persona"
                             persona-id
                             "edit")))
            (push sync-rec sync-recs)))
        (wait-for (turtl::insert-sync-records sync-recs)
          (format t "Added ~a sync records" (length sync-recs))))
      (t (e) (format t "error: ~a~%" e)))))


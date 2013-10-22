(in-package :turtl)

(defparameter *db-schema*
  `(:users (:indexes (:a (:version 1)))
    :boards (:indexes (:user_id (:version 1)))
    :notes (:indexes (:board_id (:version 1)))
    :personas
     (:indexes
       (:email (:version 1)
        :user_id (:version 1)))
    :messages
     (:indexes
       (:get_messages_to
         (:version 2
          :function ,(r:fn (m)
                       (list (:attr m "to")
                             (:attr m "id") )))
        :get_messages_from
         (:version 2
          :function ,(r:fn (m)
                       (list (:attr m "from")
                             (:attr m "id") )))))
    :challenges
     (:indexes
       (:search
         (:version 2
          :function ,(r:fn (c)
                       (list (:attr c "type")
                             (:attr c "item_id"))))))
    :invites (:indexes (:code (:version 1)))
    :boards_personas_link
     (:indexes
       (:board_id (:version 1)
        :to (:version 1)
        :from (:version 1)))
    :keychain (:indexes (:user_id (:version 1)))
    :sync
     (:indexes
       (:user_search
         (:version 1
          :function ,(r:fn (s)
                       (list (:attr s "user_id")
                             (:attr s "type")
                             (:attr s "id") )))
        :item_id (:version 1))))
  "Holds our entire db/table/index schema. Tables are are created if they don't
   exist. Indexes are also created/updated if they don't exist or if the version
   doesn't match. Index names store the version in them, ie the index `user_id`
   will actually be stored as `user_id.1` which allows us to know whether or not
   the index has the correct version.")

(defun test-sync ()
(as:with-event-loop (:catch-app-errors t)
  (future-handler-case
    (alet* ((res (sync-user-items "5241e230735ca4892c000002" "526491f3735ca4190c00000a" "user" "users"))
            ;(sock (db-sock))
            ;(query (r:r
            ;         (:map
            ;           (:eq-join
            ;             (:between
            ;                  (:table "sync")
            ;                  (list "5241e230735ca4892c000002" "user" "526491f3735ca4190c00000a")
            ;                  (list "5241e230735ca4892c000002" "user" "z")
            ;                  :index (db-index "sync" "user_search")
            ;                  :left-bound "open")
            ;             "item_id"
            ;             (:table "users"))
            ;           (r:fn (r)
            ;             (:merge
            ;               (:without (:attr r "right") "body")
            ;               `(("sync_id" . ,(:attr (:attr r "left") "id"))))))))
            ;(cursor (r:run sock query))
            ;(res (r:to-array sock cursor))
            )
      ;(r:stop/disconnect sock cursor)
      (pprint res))
    (t (e) (format t "err: ~a~%" e))))
)
            

(in-package :turtl)

(defparameter *db-schema*
  `(:users
     (:indexes
       (:a (:version 1)
        :invite_code (:version 1)))
    :boards
     (:indexes
       (:user_id (:version 1)
        :parent_id (:version 1)))
    :notes
     (:indexes
       (:boards
         (:version 2
          :function ,(r:fn (n)
                       (reql::default (:attr n "boards")
                                      (list (:attr n "board_id"))))
          :multi t)
        :user_id (:version 1)))
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
    :keychain
     (:indexes
       (:user_id (:version 1)
        :item_id (:version 1)))
    :sync
     (:indexes
       (:scan_user
         (:version 1
          :function ,(r:fn (s)
                       (:map (:attr s "rel")
                             (r:fn (user)
                               (list user (:attr s "id")))))
          :multi t)))
    :log (:indexes (:hash (:version 1)))
    :promo (:indexes (:code (:version 1))))
  "Holds our entire db/table/index schema. Tables are are created if they don't
   exist. Indexes are also created/updated if they don't exist or if the version
   doesn't match. Index names store the version in them, ie the index `user_id`
   will actually be stored as `user_id.1` which allows us to know whether or not
   the index has the correct version.")

(defparameter *analytics-schema*
  `(:events (:indexes (:event (:version 1))))
  "Holds the schema for the analytics data.")


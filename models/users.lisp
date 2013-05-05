(in-package :tagit)

(defvalidator validate-user
  (("id" :type string :required t :length 24)
   ("a" :type string :required t)
   ("body" :type cl-async-util:bytes-or-string)))

(defafun check-auth (future) (auth-key)
  "Check if the given auth key exists. Finishes with the user id if so, nil
   otherwise."
  (alet* ((auth-key (decode-key auth-key))
          (sock (db-sock))
          (query (r:r (:pluck
                        (:filter
                          (:table "users")
                          `(("a" . ,auth-key)))
                        "id")))
          (cursor (r:run sock query)))
    ;; NOTE: normally it's a faux pas to disconnect the sock if you intend to use
    ;; it with a cursor, but in this case, the cursor WILL have the first result,
    ;; so fuck it.
    (r:disconnect sock)
    (if (r:has-next cursor)
        (alet ((rec (r:next sock cursor)))
          (finish future rec))
        (finish future nil))))
        
(defafun add-user (future) (user-data)
  "Add a new user"
  (add-id user-data)
  (alet ((user (check-auth (gethash "a" user-data))))
    (if user
        (signal-error future (make-instance 'user-exists
                                            :msg "You are joining with existing login credentials. Did you mean to log in?"))
        (validate-user (user-data future)
          (alet* ((sock (db-sock))
                  (query (r:r (:insert
                                (:table "users")
                                user-data)))
                  (nil (r:run sock query)))
            (r:disconnect sock)
            (finish future user-data))))))


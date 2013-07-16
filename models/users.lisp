(in-package :tagit)

(define-condition user-exists (tagit-error)
  ((code :initform 403)))

(define-condition user-mismatch (tagit-error)
  ((code :initform 403)))

(defvalidator validate-user
  (("id" :type string :required t :length 24)
   ("a" :type string :required t)
   ("body" :type cl-async-util:bytes-or-string)))

(defafun check-auth (future) (auth-key)
  "Check if the given auth key exists. Finishes with the user id if so, nil
   otherwise."
  (alet* ((auth-key (decode-key auth-key))
          (sock (db-sock))
          ;; TODO: implement (:pluck ... "id") once >= RDB 1.8
          (query (r:r 
                        (:get-all
                          (:table "users")
                          auth-key
                          :index "a")
                        ))
          (cursor (r:run sock query))
          (res (r:to-array sock cursor)))
    (when (r:cursorp cursor)
      (wait-for (r:stop sock cursor)
        (r:disconnect sock)))
    (if (and res (< 0 (length res)))
        (let ((user (aref res 0)))
          (finish future user))
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

(defafun edit-user (future) (user-id mod-user-id user-data)
  "Edit a user. Mainly used to update a user's private (encrypted) data and
   settings."
   (if (string= user-id mod-user-id)
       (validate-user (user-data future :edit t)
         (alet* ((sock (db-sock))
                 (query (r:r (:update
                               (:get (:table "users") user-id)
                               user-data)))
                 (nil (r:run sock query)))
           (r:disconnect sock)
           (finish future user-data)))
       (signal-error future (make-instance 'user-mismatch
                                           :msg "You tried to edit someone else's account. For shame."))))

(defafun get-user-data (future) (user-id)
  "Get the private data section (`body` key) for a user."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:get (:table "users") user-id)
                        "body")))
          (user (r:run sock query)))
    (r:disconnect sock)
    (finish future user)))


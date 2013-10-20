(in-package :turtl)

(defvalidator validate-keychain-entry
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("type" :type string :required t :max-length 24)
   ("item_id" :type string :required t :length 24)
   ("key" :type string :required t)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defafun get-keychain-entry-by-id (future) (key-id)
  "Get a keychain entry by its id."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "keychain") key-id)))
          (entry (r:run sock query)))
    (r:disconnect sock)
    (finish future entry)))

(defafun get-user-keychain (future) (user-id)
  "Get all keychain entries for a user."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:get-all
                       (:table "keychain")
                       user-id
                       :index (db-index "keychain" "user_id"))
                     (r:fn (k)
                       (:~ (:has-fields k "deleted"))))))
          (cursor (r:run sock query))
          (entries (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future entries)))

(defafun add-keychain-entry (future) (user-id key-data)
  "Add a new keychain entry for the given user."
  (setf (gethash "user_id" key-data) user-id)
  (add-id key-data)
  (add-mod key-data)
  (validate-keychain-entry (key-data future)
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "keychain")
                          key-data)))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future key-data))))

(defafun delete-keychain-entry (future) (user-id key-id)
  "Delete a keychain entry."
  ;; check that the user owns it first
  (alet* ((entry (get-keychain-entry-by-id key-id)))
    (if (string= (gethash "user_id" entry) user-id)
        (alet* ((sock (db-sock))
                (query (r:r (:update
                              (:get (:table "keychain") key-id)
                              `(("mod" . ,(get-timestamp))
                                ("deleted" . t)))))
                (nil (r:run sock query)))
          (r:disconnect sock)
          (finish future t))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "You're trying to delete a keychain entry that isn't yours.")))))
                         


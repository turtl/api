(in-package :turtl)

(defvalidator validate-keychain-entry
  (("id" :type id :required t)
   ("user_id" :type id :required t)
   ("type" :type string :required t :max-length 24)
   ("item_id" :type string :required t)
   ("body" :type string :required t))
  :old t)

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
  (validate-keychain-entry (key-data future)
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "keychain")
                          key-data)))
            (nil (r:run sock query))
            (sync-ids (add-sync-record user-id
                                       "keychain"
                                       (gethash "id" key-data)
                                       "add")))
      (r:disconnect sock)
      (setf (gethash "sync_ids" key-data) sync-ids)
      (finish future key-data))))

(defafun edit-keychain-entry (future) (user-id key-id key-data)
  "Edit a keychain entry."
  (alet* ((entry (get-keychain-entry-by-id key-id)))
    (if entry
        (if (string= (gethash "user_id" entry) user-id)
            (validate-keychain-entry (key-data future :edit t)
              (setf (gethash "id" key-data) key-id)
              (remhash "user_id" key-data)
              (alet* ((sock (db-sock))
                      (query (r:r (:update
                                    (:get (:table "keychain") key-id)
                                    key-data)))
                      (nil (r:run sock query))
                      (sync-ids (add-sync-record user-id "keychain" key-id "edit")))
                (r:disconnect sock)
                (setf (gethash "sync_ids" key-data) sync-ids)
                (finish future key-data)))
            (signal-error future (make-instance 'insufficient-privileges
                                                :msg "You're trying to edit a keychain entry that isn't yours.")))
        (signal-error future (make-instance 'not-found
                                            :msg "Keychain entry not found.")))))

(adefun delete-keychain-entry (user-id key-id)
  "Delete a keychain entry."
  ;; check that the user owns it first
  (alet* ((entry (get-keychain-entry-by-id key-id)))
    (if entry
        (if (string= (gethash "user_id" entry) user-id)
            (alet* ((sock (db-sock))
                    (query (r:r (:delete (:get (:table "keychain") key-id))))
                    (nil (r:run sock query))
                    (sync-ids (add-sync-record user-id "keychain" key-id "delete")))
              (r:disconnect sock)
              sync-ids)
            (error (make-instance 'insufficient-privileges
                                  :msg "You're trying to delete a keychain entry that isn't yours.")))
        ;; no entry? fail silently
        #())))

(defafun delete-keychain-entries (future) (user-id item-id)
  "Delete all keychain entries that are attached to the given item ID."
  (alet* ((sock (db-sock))
          (query (r:r (:get-all (:table "keychain") item-id :index (db-index "keychain" "item_id"))))
          (cursor (r:run sock query))
          (entries (r:to-array sock cursor))
          (sync-records nil))
    (if (zerop (length entries))
        (progn
          (r:disconnect sock)
          (finish future 0))
        (wait (adolist (entry (coerce entries 'list))
                    (push (make-sync-record user-id "keychain" (gethash "id" entry) "delete") sync-records))
          (wait (insert-sync-records sync-records)
            (alet* ((query (r:r (:delete (:get-all (:table "keychain") item-id :index (db-index "keychain" "item_id")))))
                    (nil (r:run sock query)))
              (r:disconnect sock)
              (finish future (length entries))))))))


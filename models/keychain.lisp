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

(adefun get-keychain-entries-by-item-ids (user-id item-ids)
  "Get all keychain entries for the given user ID/item IDs."
  (alet* ((sock (db-sock))
          (query (r:r
                   ;; TODO: compound index maybe??
                   (:filter
                     (:get-all
                       (:table "keychain")
                       item-ids
                       :index (db-index "keychain" "item_id"))
                     (r:fn (entry)
                       (:== user-id (:attr entry "user_id"))))))
          (cursor (r:run sock query))
          (entries (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    entries))

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

(adefun do-delete-keychain-entry (user-id key-id)
  "Run a keychain delete."
  (alet* ((sock (db-sock))
          (query (r:r (:delete (:get (:table "keychain") key-id))))
          (nil (r:run sock query))
          (sync-ids (add-sync-record user-id "keychain" key-id "delete")))
    (r:disconnect sock)
    sync-ids))
  
(adefun delete-keychain-entry (user-id key-id)
  "Delete a keychain entry w/ permissions check."
  ;; check that the user owns it first
  (alet* ((entry (get-keychain-entry-by-id key-id)))
    (if entry
        (if (string= (gethash "user_id" entry) user-id)
            (do-delete-keychain-entry user-id key-id)
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

(adefun delete-keychain-tree (user-id board-id)
  "Grab a board's tree data (board, child boards, all notes contained therein)
   and for each item we no longer have at least read access to, remove that
   item's keychain entry (if it exists)."
  (multiple-promise-bind (boards notes)
      (get-board-tree
        board-id
        :user-id user-id
        :perm-filter (lambda (type user-id data board-perms)
                       (case type
                         (:board 
                           (let* ((cur-board-id (gethash "id" data))
                                  (perm-entry (gethash cur-board-id board-perms)))
                             ;; remove any boards we still have some level of
                             ;; permissions for. this includes the board being
                             ;; unshared
                             (and perm-entry
                                  (< 0 (gethash "perms" perm-entry 0)))))
                         (:note
                           (user-can-read-note-p user-id data board-perms)))))
    (alet* ((item-ids (loop for item across (concatenate 'vector boards notes)
                            collect (gethash "id" item)))
            (keychain-entries (get-keychain-entries-by-item-ids user-id item-ids))
            (sock (db-sock))
            (query (r:r
                     (:delete
                       (:get-all
                         (:table "keychain")
                         (map 'list (lambda (k) (gethash "id" k)) keychain-entries)))))
            (nil (r:run sock query))
            (sync-records (map 'vector
                               (lambda (ke) (make-sync-record user-id "keychain" (gethash "id" ke) "delete"))
                               keychain-entries))
            (nil (insert-sync-records sync-records)))
      ;; return the sync id(s)
      (map 'vector (lambda (s) (gethash "id" s)) sync-records))))
            


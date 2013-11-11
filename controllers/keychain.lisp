(in-package :turtl)

(defroute (:post "/api/keychain") (req res)
  "Add a keychain entry."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (key-data (post-var req "data"))
            (key (add-keychain-entry user-id key-data)))
      (send-json res key))))

(defroute (:put "/api/keychain/([0-9a-f]+)") (req res args)
  "Edit a keychain entry. Not really needed since generally an entry is just
   deleted and recreated when it needs to change, but this is here for clients
   who follow the CRUD pattern."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (key-id (car args))
            (key-data (post-var req "data"))
            (key (edit-keychain-entry user-id key-id key-data)))
      (send-json res key))))

(defroute (:delete "/api/keychain/([0-9a-f]+)") (req res args)
  "Delete a keychain entry."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (key-id (car args))
            (sync-ids (delete-keychain-entry user-id key-id)))
      (let ((hash (make-hash-table :test #'equal)))
        (setf (gethash "sync_ids" hash) sync-ids)
        (send-json res hash)))))


(in-package :turtl)

(defroute (:post "/api/keychain") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (key-data (post-var req "data"))
            (key (add-keychain-entry user-id key-data)))
      (send-json res key))))

(defroute (:put "/api/keychain/([0-9a-f]+)") (req res args)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (key-id (car args))
            (key-data (post-var req "data"))
            (key nil))
      (send-json res key))))

(defroute (:delete "/api/keychain/([0-9a-f]+)") (req res args)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (key-id (car args))
            (nil (delete-keychain-entry user-id key-id)))
      (send-json res t))))


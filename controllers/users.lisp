(in-package :turtl)

(defroute (:post "/api/users") (req res)
  (catch-errors (res)
    (alet* ((user-data (post-var req "data"))
            (user (add-user user-data)))
      (track "user-join")
      (send-json res user))))

(defroute (:post "/api/auth") (req res)
  (catch-errors (res)
    (send-json res (user-id req))))

(defroute (:put "/api/users/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (mod-user-id (car args))
            (user-data (post-var req "data"))
            (user (edit-user user-id mod-user-id user-data)))
      (send-json res user))))


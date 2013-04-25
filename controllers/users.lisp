(in-package :tagit)

(defroute (:post "/api/users") (req res)
  (catch-errors (res)
    (alet* ((user-data (post-var req "data"))
            (user (add-user user-data)))
      (send-json res user))))

(defroute (:post "/api/auth") (req res)
  (catch-errors (res)
    (send-json res (user-id req))))

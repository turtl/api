(in-package :tagit)

(defroute (:post "/users") (req res)
  (catch-errors (res)
    (alet* ((user-data (post-var req "data"))
            (user (add-user user-data)))
      (send-json res user))))

(defroute (:post "/auth") (req res)
  (catch-errors (res)
    (send-json res t)))

(in-package :turtl)

(defroute (:post "/api/log/error") (req res)
  "Log a client error. Used for debugging."
  (catch-errors (res)
    (alet* ((log-data (post-var req "data"))
            (nil (add-log log-data)))
      (send-json res "logged."))))


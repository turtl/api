(in-package :turtl)

(defroute (:post "/api/log/error") (req res)
  "Log a client error. Used for debugging."
  (catch-errors (res)
    (alet* ((log-data (post-var req "data"))
            (data (add-log log-data)))
      (track "error" `(:hash ,(gethash "id" data)) req)
      (send-json res "logged."))))


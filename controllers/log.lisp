(in-package :turtl)

(route (:post "/log/error") (req res)
  "Log a client error. Used for debugging."
  (alet* ((log-data (post-var req "data"))
          (data (add-log log-data)))
    (when data
      (track "error" `(:hash ,(gethash "id" data)) req))
    (send-json res "logged.")))


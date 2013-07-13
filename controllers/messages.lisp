(in-package :tagit)

(defroute (:get "/api/messages/personas/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (after (get-var req "after"))
            (challenge-response (get-var req "challenge"))
            (messages (get-messages-for-persona persona-id challenge-response :after after)))
      (send-json res messages))))

(defroute (:post "/api/messages") (req res)
  (catch-errors (res)
    (alet* ((message-data (post-var req "data"))
            (challenge (post-var req "challenge"))
            (message (send-message message-data challenge)))
      (send-json res message))))


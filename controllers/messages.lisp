(in-package :tagit)

(defroute (:get "/api/messages/persona/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (after (get-var req "after"))
            (challenge-response (get-var req "challenge"))
            (messages (get-messages-for-persona persona-id challenge-response :after after)))
      (send-json res messages))))

(defroute (:post "/api/messages") (req res)
  (catch-errors (res)
    (alet* ((from-persona-id (post-var req "from"))
            (from-persona-challenge (post-var req "challenge"))
            (to-persona-id (post-var req "to"))
            (body (post-var req "body"))
            (message (send-message from-persona-id from-persona-challenge to-persona-id body)))
      (send-json res (gethash "id" message)))))


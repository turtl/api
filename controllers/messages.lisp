(in-package :turtl)

(route (:get "/messages/personas/([0-9a-f-]+)") (req res args)
  "Get a persona's messages."
  (alet* ((user-id (user-id req))
          (persona-id (car args))
          (after (get-var req "after"))
          (messages (get-messages-for-persona user-id persona-id :after after)))
    (send-json res messages)))

(route (:post "/messages") (req res)
  "Send a new message."
  (alet* ((user-id (user-id req))
          (message-data (post-var req "data"))
          (message (send-message user-id message-data)))
    (track "message-send" nil req)
    (send-json res message)))

(route (:delete "/messages/([0-9a-f-]+)") (req res args)
  "Delete a message."
  (alet* ((user-id (user-id req))
          (message-id (car args))
          (persona-id (post-var req "persona"))
          (nil (delete-message user-id message-id persona-id)))
    (track "message-delete" nil req)
    (send-json res t)))


(in-package :turtl)

(route (:post "/feedback") (req res)
  "Gather feedback and email it to the Turtl admins."
  (alet* ((user-id (user-id req))
          (feedback-data (post-var req "data"))
          (client (get-client req))
          (feedback (send-feedback user-id feedback-data client)))
    (track "feedback" nil req)
    (send-json res feedback)))


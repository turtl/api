(in-package :turtl)

(route (:post "/feedback") (req res)
  "Gather feedback and email it to the Turtl admins."
  (alet* ((user-id (user-id req))
          (feedback-data (post-var req "data"))
          (feedback (send-feedback user-id feedback-data)))
    (track "feedback" nil req)
    (send-json res feedback)))


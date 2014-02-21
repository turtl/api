(in-package :turtl)

(defroute (:post "/api/feedback") (req res)
  "Gather feedback and email it to the Turtl admins."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (feedback-data (post-var req "data"))
            (feedback (send-feedback user-id feedback-data)))
      (track "feedback" nil req)
      (send-json res feedback))))


(in-package :turtl)

(defvalidator validate-feedback
  (("user_id" :type id :required t)
   ("email" :type string :required t)
   ("body" :type string :required t))
  :old t)

(defafun send-feedback (future) (user-id feedback-data)
  "Send feedback posted by a user to the proper channels (email, most likely)."
  (setf (gethash "user_id" feedback-data) user-id)
  (unless (gethash "email" feedback-data)
    (setf (gethash "email" feedback-data) "nobody@turtl.it"))
  (validate-feedback (feedback-data future)
    (alet ((sentp (email-feedback feedback-data)))
      (finish future sentp))))


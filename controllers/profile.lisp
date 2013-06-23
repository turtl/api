(in-package :tagit)

(defroute (:get "/api/profiles/users/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (let ((user-id (car args)))
      (unless (string= (user-id req) user-id)
        (error 'insufficient-privileges :msg "You are trying to access another user's projects. For shame."))
      (alet* ((projects (get-user-projects user-id t))
              (settings nil)
              (response (make-hash-table :test #'equal)))
        (setf (gethash "projects" response) projects
              (gethash "settings" response) settings)
        (send-json res response)))))


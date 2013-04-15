(in-package :tagit)

(defroute (:get "/api/projects/([0-9a-f-]+)/notes/user/([0-9a-f-]+)") (req res args)
  ;; TODO: AUTH: make sure user-id == (user-id req)
  (alet* ((project-id (car args))
          (user-id (cadr args))
          (notes (get-user-notes user-id project-id)))
    (send-json res notes)))


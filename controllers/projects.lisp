(in-package :tagit)

(defroute (:get "/api/projects/user/([0-9a-f-]+)") (req res args)
  ;; TODO: AUTH: make sure user-id == (user-id req)
  (alet* ((user-id (car args))
          (projects (get-user-projects user-id)))
    (send-json res projects)))

(defroute (:post "/api/projects/user/([0-9a-f-]+)") (req res args)
  ;; TODO: AUTH: make sure user-id == (user-id req)
  (alet* ((user-id (car args))
          (project-json (http-parse:http-body (request-http req)))
          (project-data (yason:parse (babel:octets-to-string project-json :encoding :utf-8))))
    (alet ((project (add-project user-id project-data)))
      (send-json res project))))


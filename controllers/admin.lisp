(in-package :turtl)

(defparameter *admin-page*
  (file-contents (concatenate 'string (namestring *root*) "views/admin.html"))
  "Holds the admin page.")

(defroute (:get "/admin") (req res)
  "Get the admin page, populated with our data."
  (catch-errors (res)
    (alet* ((admin-stats (get-admin-stats))
            (html (populate-stats *admin-page* admin-stats)))
      (send-response res :body html))))


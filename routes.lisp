(in-package :tagit)

;; clear out all routes (start anew)
(clear-routes)

;; load our models. shouldn't really go here
(load-folder (concatenate 'string (namestring *root*) "models/"))

;; load all our controllers (and their contained routes)
(load-folder (concatenate 'string (namestring *root*) "controllers/"))

(defroute (:get "/refresh-views") (req res)
  (load-views)
  (send-response res :body "Views refreshed!!"))

(defroute (:get "/favicon.ico") (req res)
  (send-response res :status 301 :headers '(:location "/favicon.png")))

;; set up a general file-serving route
(def-directory-route "/" (format nil "~awebroot" *root*)
                     :disable-directory-listing t)

;; set up a catch-all route which loads the app, no matter the URL
(defroute (:get ".+") (req res)
  (let ((body (layout :default '(:content "" :title "tag.it"))))
    (send-response res :headers '(:content-type "text/html") :body body)))


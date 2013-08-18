(in-package :turtl)

;; clear out all routes (start anew)
(clear-routes)

;; load our models. shouldn't really go here
(load-folder (concatenate 'string (namestring *root*) "models/"))

;; load all our controllers (and their contained routes)
(load-folder (concatenate 'string (namestring *root*) "controllers/"))

(defroute (:* "/api/.+") (req res)
  (send-response res
                 :status 404
                 :headers '(:content-type "application/json")
                 :body (to-json "Unknown resource.")))
                 
(defroute (:get "/favicon.ico") (req res)
  (send-response res :status 301 :headers '(:location "/favicon.png") :body ""))

;; set up a general file-serving route
(def-directory-route "/" *site-assets*
                     :disable-directory-listing t)

;; set up a catch-all route which loads the app, no matter the URL
(defroute (:get ".+") (req res)
  (let ((body (layout :default '(:content "" :title "Turtl"))))
    (send-response res :headers '(:content-type "text/html") :body body)))

(defroute (:* ".+") (req res)
  (send-response res :body "Page not found." :status 404))


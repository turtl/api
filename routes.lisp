(in-package :tagit)

;; clear out all routes (start anew)
(clear-routes)

(def-directory-route "/" (format nil "~awebroot" *root*)
                     :disable-directory-listing t)

(defroute (:get "/refresh-views") (req res)
  (load-views)
  (send-response res :body "Views refreshed!!"))

(defroute (:get "/favicon.ico") (req res)
  (send-response res :status 301 :headers '(:location "/favicon.png")))

(defroute (:get ".+") (req res)
  (let ((body (layout :default '(:content "" :title "tag.it"))))
    (send-response res :headers '(:content-type "text/html") :body body)))


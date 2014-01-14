(in-package :turtl)

;; Basically here to support the Firefox addon...
;; TODO: send a real options response for each URL by reading the routing table.
(add-hook :pre-route
  (lambda (req res)
    (let ((future (make-future)))
      (if (eq (request-method req) :options)
          (progn
            (send-response res
                           :status 200
                           :headers '(:content-length 0
                                      :allow "OPTIONS, GET, POST, PUT, DELETE"))
            (signal-error future (make-instance 'as:tcp-info)))
          (finish future))
      future))
  :options-support)

(defroute (:* "/api/.+") (req res)
  "Any /api/* route that lands here wasn't caught by our controllers. Send the
   client a nice 404 and be done with it."
  (send-response res
                 :status 404
                 :headers '(:content-type "application/json")
                 :body (to-json "Unknown resource.")))

(defroute (:get "/favicon.ico") (req res)
  "Who uses .ico??"
  (send-response res :status 301 :headers '(:location "/favicon.png") :body ""))

;; if we're handling local file uploads, define a route to the upload dir.
(when *local-upload*
  (def-directory-route "/files" *local-upload* :disable-directory-listing t))

;; only turn these on if the webapp is enabled
(when *enable-webapp*
  ;; set up a general file-serving route (hint: if you're in production and you
  ;; get here, you're doing it wrong)
  (def-directory-route "/" *site-assets* :disable-directory-listing t)
  
  ;; TODO: remove this at some point, or password it or something.
  (defroute (:get ".+") (req res)
    "This is our catch-all route which loads the Turtl web app."
    (let ((body (layout :default '(:content "" :title "Turtl"))))
      (send-response res :headers '(:content-type "text/html") :body body))))

(defroute (:* ".+") (req res)
  "- What you doing mister?
   - Nothing.
   - Yes you are, you're tresspassing on private property.
   - Tresspassing?
   - You're loitering too, man.
   - That's right, you're loitering too."
  (send-response res :body "Page not found." :status 404))


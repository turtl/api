(in-package :turtl)

;; Basically here to support the Firefox addon...
;; TODO: send a real options response for each URL by reading the routing table.
(add-hook :pre-route
  (lambda (req res)
    (with-promise (resolve reject)
      (if (eq (request-method req) :options)
          (progn
            (send-response res
                           :status 200
                           :headers '(:content-length 0
                                      :allow "OPTIONS, GET, POST, PUT, DELETE"))
            (reject (make-instance 'as:tcp-info)))
          (resolve))))
  :options-support)

(defroute (:* "/api/.+") (req res)
  "Any /api/* route that lands here wasn't caught by our controllers. Send the
   client a nice 404 and be done with it."
  (send-response res
                 :status 404
                 :headers '(:content-type "application/json")
                 :body (to-json "Unknown resource.")))

;; if we're handling local file uploads, define a route to the upload dir.
(when *local-upload*
  (def-directory-route "/files" *local-upload* :disable-directory-listing t)
  ;; if the directory route doesn't pick up a file and we're doing local uploads
  ;; and downloads, signal a 404
  (defroute (:get "/files/.+") (req res)
    (send-response res :status 404 :body "\"File not found\"")))

(defroute (:* ".+") (req res)
  "- What you doing mister?
   - Nothing.
   - Yes you are, you're tresspassing on private property.
   - Tresspassing?
   - You're loitering too, man.
   - That's right, you're loitering too."
  (send-response res :body "Page not found." :status 404))


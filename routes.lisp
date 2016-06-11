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

(defroute (:post "/cla/sign") (req res)
  "Someone wants to sign our CLA! What a glorious day this is."
  (let* ((redirect-err (post-var req "redirect-err"))
         (redirect-err (if (or (not redirect-err)
                               (string= redirect-err ""))
                           "https://turtl.it/contributing/sign-error"
                           redirect-err)))
    (catcher
      (alet* ((fields (list "type"
                            "entity"
                            "fullname"
                            "email"
                            "address1"
                            "address2"
                            "city"
                            "state"
                            "zip"
                            "country"
                            "phone"
                            "github"
                            "sign"))
              (redirect (post-var req "redirect"))
              (redirect (if (or (not redirect)
                                (string= redirect ""))
                            "https://turtl.it/contributing/sign-thanks"
                            redirect))
              (data (let ((hash (hash)))
                      (dolist (field fields)
                        (setf (gethash field hash) (post-var req field)))
                      hash))
              (nil (cla-sign data)))
        (send-response res
                       :status 302
                       :headers (list :location redirect)
                       :body "Thanks for signing! Redirecting..."))
      (error (e)
        (vom:error "Caught error: ~a" e)
        (print-backtrace e)
        (unless (wookie:response-finished-p res)
          (unless (as:socket-closed-p (get-socket res))
            (send-response res
                           :status 302
                           :headers (list :location redirect-err)
                           :body "There was an error processing your signature. Redirecting.")))))))

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


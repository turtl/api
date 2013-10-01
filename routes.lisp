(in-package :turtl)

(defun define-app-routes ()
  ;; clear out all routes (start anew)
  (clear-routes)
  ;; load our models. note we specify a load order to cut back on "UNDEFINED
  ;; FUNCTION LOL" errors.
  (load-folder (concatenate 'string (namestring *root*) "models/")
               '("users"
                 "challenges"
                 "email"
                 "personas"
                 "notes"
                 "boards"
                 "messages"
                 "invites"
                 "sync"))

  ;; load all our controllers. this will load all our API routes
  (load-folder (concatenate 'string (namestring *root*) "controllers/"))
  
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
  
  ;; set up a general file-serving route (hint: if you're in production and you
  ;; get here, you're doing it wrong)
  (def-directory-route "/" *site-assets* :disable-directory-listing t)
  
  ;; TODO: remove this at some point, or password it or something.
  (defroute (:get ".+") (req res)
    "This is our catch-all route which loads the Turtl web app."
    (let ((body (layout :default '(:content "" :title "Turtl"))))
      (send-response res :headers '(:content-type "text/html") :body body)))
  
  (defroute (:* ".+") (req res)
    "- What you doing mister?
     - Nothing.
     - Yes you are, you're tresspassing on private property.
     - Tresspassing?
     - You're loitering too, man.
     - That's right, you're loitering too."
    (send-response res :body "Page not found." :status 404)))


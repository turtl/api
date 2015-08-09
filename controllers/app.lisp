(in-package :turtl)

;; this is responsible for checking user auth
;; TODO: if this ever does MORE than just check auth, be sure to split into
;;       multiple functions
(add-hook :pre-route
  (lambda (req res)
    (with-promise (res rej)
      (let* ((auth (get-header (request-headers req) :authorization))
             (path (quri:uri-path (request-uri req)))
             (method (request-method req))
             (auth-fail-fn (lambda ()
                             (let ((err (make-instance 'auth-failed :msg "Authentication failed."))
                                   ;; random wait time (0-2ms) to prevent timing attacks on auth
                                   (rand-wait (/ (secure-random:number 2000000) 100000000d0)))
                               (as:delay
                                 (lambda ()
                                   (send-response res
                                                  :status (error-code err)
                                                  :headers '(:content-type "application/json")
                                                  :body (error-json err))
                                   (rej err))
                                 :time rand-wait)))))
        (if (or (is-public-action method path)
                (eq (request-method req) :options))
            ;; this is a signup or file serve. let it fly with no auth
            (res)
            ;; not a signup, test the auth...
            (if auth
                (let* ((auth (subseq auth 6))
                       (auth (cl-base64:base64-string-to-string auth))
                       (split-pos (position #\: auth))
                       (auth-key (if split-pos
                                     (subseq auth (1+ split-pos))
                                     nil)))
                  (catch-errors (res)
                    (alet* ((user (check-auth auth-key)))
                      (if user
                          (progn 
                            (setf (request-data req) user)
                            (res))
                          (funcall auth-fail-fn)))))
                (funcall auth-fail-fn))))))
  :turtl-auth)

(add-hook :response-started
  (lambda (res req &rest _)
    (declare (ignore _))
    (when *enable-hsts-header*
      (setf (getf (response-headers res) :strict-transport-security)
            (format nil "max-age=~a" *enable-hsts-header*)))
    ;; set up CORS junk. generally, we only allow it if it comes from the FF
    ;; extension, which uses resource:// URLs
    (let* ((req-headers (request-headers req))
           (origin (get-header req-headers :origin)))
      ;; TODO: figure out a better CORS policy
      (when (or t
                (and origin (< 11 (length origin)) (string= (subseq origin 0 11) "resource://")))
        (setf (getf (response-headers res) :access-control-allow-origin) *enabled-cors-resources*
              (getf (response-headers res) :access-control-allow-methods) "GET, POST"
              (getf (response-headers res) :access-control-allow-headers) (get-header (request-headers req) :access-control-request-headers)))))
  :post-headers)


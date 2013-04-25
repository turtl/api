(in-package :tagit)

(defun user-id (request)
  "Grab a user id from a request."
  (getf (request-data request) :id))

;; this is responsible for checking user auth
(add-hook :pre-route
  (lambda (req res)
    (let ((future (make-future)))
      (let* ((auth (getf (request-headers req) :authorization))
             (path (puri:uri-path (request-uri req)))
             (method (request-method req)))
        (if (and (eq method :post)
                 (string= path "/users"))
            ;; this is a signup. let it fly with no auth
            (finish future)
            ;; not a signup, test the auth...
            (if auth
                (let* ((auth (subseq auth 6))
                       (auth (cl-base64:base64-string-to-string auth))
                       (split-pos (position #\: auth))
                       (auth-key (if split-pos
                                     (subseq auth (1+ split-pos))
                                     nil)))
                  (alet* ((user (check-auth auth-key)))
                    (setf (request-data req) user)))
                (let ((err (make-instance 'auth-failed :msg "Authentication failed.")))
                  (send-response res
                                 :status (error-code err)
                                 :headers '(:content-type "application/json")
                                 :body (error-json err))
                  (finish future)))))
      future)))


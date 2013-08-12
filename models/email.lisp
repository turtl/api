(in-package :tagit)

(define-condition email-send-failed (tagit-error)
  ((code :initform 500)))

(defafun send-email (future) (to subject body)
  "Send an email. Returns a future that finishes when the operation is done (or
   errors out otherwise)."
  (multiple-future-bind (res status)
      (das:http-request "https://sendgrid.com/api/mail.send.json"
                        :method :post
                        :force-binary t
                        :parameters `(("api_user" . ,*email-user*)
                                      ("api_key" . ,*email-pass*)
                                      ("to" . ,to)
                                      ("from" . ,*email-from*)
                                      ("subject" . ,subject)
                                      ("text" . ,body)))
    (if (<= 200 status 299)
        ;; success, return t
        (finish future t)
        ;; error. grab the message and signal
        (let* ((res (babel:octets-to-string res))
               (hash (yason:parse res))
               (msg (gethash "error" hash))
               (msg (if (hash-table-p msg)
                        (gethash "message" msg)
                        (car (gethash "errors" hash)))))
          (signal-error future (error 'email-send-failed :msg msg))))))


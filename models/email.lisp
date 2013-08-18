(in-package :tagit)

(define-condition email-send-failed (tagit-error)
  ((code :initform 500)))

;; -----------------------------------------------------------------------------
;; email templates
;; -----------------------------------------------------------------------------
(defparameter *emails-board-invite* (format nil "~
Hello.

{{from}} has shared a board with you on Tagit. ~
Tagit is an easy way to track, share, and collaborate on ideas and projects ~
with friends or coworkers.
 
To accept this invite, go here: {{site-url}}/invite/{{code}}?secret={{used-secret}}
  
Otherwise, you can ignore this email entirely and things will just work ~
themselves out.
  
Thanks!
Andrew, Jeff, and Drew
(the Tagit team)
"))
;; -----------------------------------------------------------------------------
;; / email templates
;; -----------------------------------------------------------------------------

(defun email-template (txt vars)
  "A simple email templating system."
  ;; split our vars plist into key/val
  (loop for (key val) on vars by #'cddr do
    (let* ((id (concatenate 'string "{{" (string-downcase (string key)) "}}"))
           (val (if (stringp val)
                    val
                    (write-to-string val))))
      (setf txt (cl-ppcre:regex-replace-all id txt val))))
  txt)

(defun obscure-email (email-addr)
  "Turn andrew.lyon@teamaol.com into and******@teamaol.com."
  (cl-ppcre:regex-replace "^(.{0,3}).+@" email-addr "\\1*****@"))

(defafun send-email (future) (to subject body &key reply-to from-name)
  "Send an email. Returns a future that finishes when the operation is done (or
   errors out otherwise)."
  (let ((params `(("api_user" . ,*email-user*)
                  ("api_key" . ,*email-pass*)
                  ("to" . ,to)
                  ("from" . ,*email-from*)
                  ("subject" . ,subject)
                  ("text" . ,body))))
    (when from-name (push `("fromname" . ,from-name) params))
    (when reply-to (push `("replyto" . ,reply-to) params))
    (multiple-future-bind (res status)
        (das:http-request "https://sendgrid.com/api/mail.send.json"
                          :read-timeout 5
                          :method :post
                          :force-binary t
                          :parameters params)
      (if (<= 200 status 299)
          ;; success, return t
          (finish future t)
          ;; error. grab the message and signal
          (let* ((res (babel:octets-to-string res))
                 (hash (yason:parse res))
                 (msg (gethash "error" hash))
                 (msg (if (hash-table-p msg)
                          (gethash "message" msg)
                          (car (gethash "errors" hash))))
                 (msg (concatenate 'string "Failed to send email: " msg)))
            (signal-error future (error 'email-send-failed :msg msg)))))))

(defafun email-board-invite (future) (from-persona invite)
  "Send a board invite email."
  (let* ((msg *emails-board-invite*)
         (name (gethash "name" from-persona))
         (name (when (and name (not (string= name "")))
                 name))
         (email (gethash "email" from-persona))
         (from (if name
                   (format nil "~a (~a)" name email)
                   email))
         (tpl-vars `(:from ,from
                     :site-url ,*site-url*
                     :code ,(gethash "code" invite)
                     :used-secret ,(if (gethash "used_secret" (gethash "data" invite))
                                       1
                                       0)))
         (subject (format nil "~a shared a board with you on Tagit" (if name name email)))
         (body (email-template msg tpl-vars))
         (to (gethash "to" invite)))
    (alet* ((sentp (send-email to subject body :reply-to email :from-name (if name name email))))
      (finish future sentp))))


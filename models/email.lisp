(in-package :turtl)

(define-condition email-send-failed (turtl-error)
  ((code :initform 500)))

;; -----------------------------------------------------------------------------
;; email templates
;; -----------------------------------------------------------------------------
(defparameter *emails-board-invite* (format nil "~
Hello.

{{from}} has shared a board with you on Turtl. ~
Turtl is an easy way to track, share, and collaborate on ideas and projects ~
with friends or coworkers.
 
To accept this invite, go here: {{site-url}}/invites/{{code}}/{{id}}/{{key}}
  
Otherwise, you can ignore this email entirely and things will just work ~
themselves out.
  
Thanks!
The Turtl Team
{{site-url}}
"))

(defparameter *emails-board-persona-invite* (format nil "~
{{greeting}}.

{{from}} has shared a board with you on Turtl. ~
Log in to the Turtl browser extension to start sharing!

You can disable these notifications by opening the Personas dialog in the Turtl ~
menu and clicking the mail icon next to your persona.

Thanks!
The Turtl Team
{{site-url}}
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

(defun get-persona-greeting (persona-data)
  "Given a persona hash object, pull out something like
     'Andrew Lyon (orthecreedence@gmail.com)'
   or
     'orthecreedence@gmail.com'"
  (let* ((name (gethash "name" persona-data))
         (name (when (and name (not (string= name "")))
                 name))
         (email (gethash "email" persona-data))
         (namestr (if name
                      (format nil "~a (~a)" name email)
                      email)))
    namestr))
     
(defafun email-board-invite (future) (from-persona invite key)
  "Send a board invite email."
  (let* ((msg *emails-board-invite*)
         (name (gethash "name" from-persona))
         (name (when (and name (not (string= name ""))) name))
         (email (gethash "email" from-persona))
         (from (get-persona-greeting from-persona))
         (tpl-vars `(:site-url ,*site-url*
                     :from ,from
                     :id ,(gethash "id" invite)
                     :code ,(gethash "code" invite)
                     :key ,key
                     :used-secret ,(if (gethash "used_secret" (gethash "data" invite))
                                       1
                                       0)))
         (subject (format nil "~a shared a board with you on Turtl" (if name name email)))
         (body (email-template msg tpl-vars))
         (to (gethash "to" invite)))
    (alet* ((sentp (send-email to subject body :reply-to email :from-name (if name name email))))
      (finish future sentp))))

(defafun email-board-persona-invite (future) (from-persona to-persona)
  "Send a board invite email to a persona."
  (let* ((msg *emails-board-persona-invite*)
         (from-name (gethash "name" from-persona))
         (from-name (when (and from-name (not (string= from-name ""))) from-name))
         (from-email (gethash "email" from-persona))
         (from (get-persona-greeting from-persona))
         (to-name (gethash "name" to-persona))
         (to-name (when (and to-name (not (string= to-name ""))) to-name))
         (to-email (gethash "email" to-persona))
         (to (get-persona-greeting to-persona))
         (tpl-vars `(:site-url ,*site-url*
                     :from ,from
                     :to ,to
                     :greeting ,(if to-name
                                    (concatenate 'string "Hi, " to-name)
                                    "Hi.")))
         (subject (format nil "~a shared a board with you on Turtl" (if from-name from-name from-email)))
         (body (email-template msg tpl-vars)))
    (alet* ((sentp (send-email to-email subject body :reply-to from-email :from-name (if from-name from-name from-email))))
      (finish future sentp))))

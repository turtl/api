(in-package :turtl)

(define-condition email-send-failed (turtl-error)
  ((code :initform 500)))

;; -----------------------------------------------------------------------------
;; email templates
;; -----------------------------------------------------------------------------
(defparameter *emails-board-email-invite* (format nil "~
Hello.

{{from}} has shared a board with you on Turtl. ~
Turtl is an easy way to securely collaborate on projects with friends or ~
coworkers.
 
To accept this invite, download the Turtl app ({{site-url}}) and after ~
installing, enter the following code into the \"Sharing\" section of the menu:

{{invite-code}}

Or you can safely ignore this email.
  
Thanks!
The Turtl Team
{{site-url}}
"))

(defparameter *emails-board-persona-invite* (format nil "~
{{greeting}}.

{{from}} has shared a board with you on Turtl. Log in to accept this invite.

You can disable these notifications by opening the Personas dialog in the Turtl ~
menu and unchecking \"Email me when someone shares with me\".

Thanks!
The Turtl Team
{{site-url}}
"))

(defparameter *emails-feedback* (format nil "~
You have received feedback from {{email}} (user id {{user-id}}, client {{client}}):

********

{{body}}

********

Please respond in a timely manner!"))

(defparameter *cla-signature* (format nil "~
Someone signed the CLA:

{{sigdata}}
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

(adefun send-email-smtp (host email-from to subject body &key reply-to display-name)
  "Send en email via SMTP."
  (with-promise (resolve reject)
    (let* ((error nil)
           (handler (lambda (&optional err)
                      (if (or err error)
                          (reject (error 'email-send-failed :msg (format nil "Sending mail through SMTP failed: ~a" (or err error))))
                          (resolve t))))
           ;; thread-safe notifier
           (trigger (as:make-notifier handler :event-cb handler)))
      (bt:make-thread
        (lambda ()
          (handler-case
            (cl-smtp:send-email host
                                email-from
                                to
                                subject
                                body
                                :reply-to reply-to
                                :display-name display-name)
            (t (e) (setf error e)))
          (as:trigger-notifier trigger))))))

(adefun send-email (to subject body &key reply-to from-name (email-from *email-from*))
  "Send an email. Returns a future that finishes when the operation is done (or
   errors out otherwise)."
  (when *smtp-host*
    (return-from send-email (send-email-smtp
                              *smtp-host*
                              email-from
                              to
                              subject
                              body
                              :reply-to (if reply-to reply-to email-from)
                              :display-name (if from-name from-name email-from))))
  (let ((params `(("api_user" . ,*email-user*)
                  ("api_key" . ,*email-pass*)
                  ("to" . ,to)
                  ("from" . ,email-from)
                  ("subject" . ,subject)
                  ("text" . ,body))))
    (when from-name (push `("fromname" . ,from-name) params))
    (when reply-to (push `("replyto" . ,reply-to) params))
    (multiple-promise-bind (res status)
        (das:http-request "https://sendgrid.com/api/mail.send.json"
                          :read-timeout 5
                          :method :post
                          :force-binary t
                          :parameters params)
      (if (<= 200 status 299)
          ;; success, return t
          t
          ;; error. grab the message and signal
          (let* ((res (babel:octets-to-string res))
                 (hash (jonathan:parse res :as :hash-table))
                 (msg (gethash "error" hash))
                 (msg (if (hash-table-p msg)
                          (gethash "message" msg)
                          (car (gethash "errors" hash))))
                 (msg (concatenate 'string "Failed to send email: " msg)))
            (error 'email-send-failed :msg msg))))))

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
     
(adefun email-board-email-invite (from-persona invite)
  "Send a board invite email."
  (let* ((msg *emails-board-email-invite*)
         (name (gethash "name" from-persona))
         (name (when (and name (not (string= name ""))) name))
         (email (gethash "email" from-persona))
         (from (get-persona-greeting from-persona))
         (invite-code (base64:string-to-base64-string
                        (concatenate 'string
                                     (gethash "id" invite)
                                     ":"
                                     (gethash "object_id" invite))))
         (tpl-vars `(:site-url ,*site-url*
                     :from ,from
                     :invite-code ,invite-code))
         (subject (format nil "~a shared a board with you on Turtl" (if name name email)))
         (body (email-template msg tpl-vars))
         (to (gethash "to" invite)))
    (send-email to subject body :reply-to email :from-name (if name name email))))

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

(defafun email-feedback (future) (feedback-data)
  "Send feedback to a Turtl admin email."
  (let* ((msg *emails-feedback*)
         (email (gethash "email" feedback-data))
         (client (gethash "client" feedback-data))
         (to-email *admin-email*)
         (body (gethash "body" feedback-data))
         (user-id (gethash "user_id" feedback-data))
         (tpl-vars `(:user-id ,user-id
                     :client ,client
                     :email ,email
                     :body ,body))
         (subject (format nil "New Turtl feedback from ~a" email))
         (body (email-template msg tpl-vars)))
    (alet ((sentp (send-email to-email subject body :email-from email :reply-to email :from-name "Turtl feedback")))
      (finish future sentp))))

(adefun email-cla (cla-data)
  "Send an email notification about a CLA signature."
  (let* ((msg *cla-signature*)
         (to-email *admin-email*)
         (sigdata (loop for k being the hash-keys of cla-data
                        for v being the hash-values of cla-data
                        collect (format nil "~a: ~a~%" k v)))
         (sigdata (apply 'concatenate (append (list 'string) sigdata)))
         (tpl-vars (list :sigdata sigdata))
         (subject (format nil "CLA signature"))
         (body (email-template msg tpl-vars)))
    (send-email to-email subject body :email-from "cla@turtl.it" :from-name "Turtl CLA Signature")))


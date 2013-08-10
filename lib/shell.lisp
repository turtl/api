;;; NOTE: don't forget to include #:external-program if this file is loaded.

(in-package :tagit)

(defun fork-shell (cmd &optional finish-cb)
  "Fork a shell command, call the finish-cb on completion, call error-cb on
   error."
  (let* ((output (make-string-output-stream))
         (status-fn (lambda (proc)
                      (unless (eql (external-program:process-status proc) :running)
                        (when finish-cb
                          (funcall finish-cb (nth-value 1 (external-program:process-status proc)) (get-output-stream-string output))))))
         (process (external-program:start "bash" (list "-c" cmd) :output output :error :output :status-hook status-fn)))
    process))

(defun send-mail-shell (to subject body &optional finish-cb)
  "Send an email."
  (flet ((escape (str &key remove-nl)
           (let* ((esc str)
                  (esc (cl-ppcre:regex-replace-all "\"" esc "\\\""))
                  (esc (if remove-nl
                           (cl-ppcre:regex-replace-all "\\\\n" esc " ")
                           esc)))
             esc)))
    (let* ((cmd (format nil "~
echo -e \"~
To: ~a\\n~
From: noreply@tagit.beeets.com\\n~
Subject: ~a\\n~
~a\\n\\n\" | msmtp ~a" (escape to :remove-nl t)
                       (escape subject :remove-nl t)
                       (escape body)
                       to)))
      ;(return-from send-mail cmd)
      (fork-shell cmd finish-cb))))


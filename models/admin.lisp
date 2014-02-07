(in-package :turtl)

(defun populate-stats (html stats)
  "Given an HTML string, populate the {{stat:statname}} portions of it with the
   given data."
  (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner "{{stat:([a-z0-9_-]+)}}" :multi-line-mode t)
    html
    (lambda (match &rest regs)
      (declare (ignore match))
      (let* ((regs (cddddr regs))
             (rs (car regs))
             (re (cadr regs))
             (name (string-upcase (subseq html (aref rs 0) (aref re 0))))
             (name-keyword (intern name :keyword))
             (stat (getf stats name-keyword))
             (stat (round stat)))
        ;; write stat to string
        (format nil "~a" stat)))))

(defun populate-log (html logs)
  "Push the given logs into the admin HTML."
  (let ((log-html (with-output-to-string (s)
                    (loop for entry across logs do
                      (let* ((log-data (gethash "data" entry))
                             (url (gethash "url" log-data))
                             (url (if (string= url "")
                                      "unknown"
                                      url)))
                        (format s "
<li>
  <div class=\"summary\">
    <span class=\"count\">~a</span>
    <span class=\"id\">~a</span>
    <span class=\"version\">~a</span>
    <span class=\"file\">~a:~a</span>
  </div>
  <div class=\"expanded\">
    ~a
  </div>
</li>"
                                (round (gethash "c" entry))
                                (gethash "id" entry)
                                (gethash "version" log-data)
                                url
                                (gethash "line" log-data)
                                (gethash "msg" log-data)))))))
    (cl-ppcre:regex-replace
      "{{logs}}"
      html
      log-html)))

(defafun get-admin-stats (future) ()
  "Grab statistics for the admin page."
  (macrolet ((count-not-deleted (table)
               `(:count (:filter
                          (:table ,table)
                          (r:fn (item)
                            (:|| (:~ (:has-fields item "deleted"))
                                 (:== (:attr item "deleted") nil)))))))
    (alet* ((sock (db-sock))
            (num-users (r:run sock (r:r (count-not-deleted "users"))))
            (num-boards (r:run sock (r:r (count-not-deleted "boards"))))
            (num-notes (r:run sock (r:r (count-not-deleted "notes"))))
            (num-personas (r:run sock (r:r (count-not-deleted "personas"))))
            (num-personas-wo-rsa (r:run sock (r:r (:count
                                                    (:filter (:table "personas")
                                                      (r:fn (p)
                                                        (:&& (:== (:default (:attr p "pubkey") nil)
                                                                  nil)
                                                             (:|| (:~ (:has-fields p "deleted"))
                                                                  (:== (:attr p "deleted") nil))))))))))
      (r:disconnect sock)
      (finish future (list :num-users num-users
                           :num-boards num-boards
                           :num-notes num-notes
                           :num-personas num-personas
                           :num-personas-wo-rsa num-personas-wo-rsa)))))


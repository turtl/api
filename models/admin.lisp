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

(defafun get-admin-stats (future) ()
  "Grab statistics for the admin page."
  (alet* ((sock (db-sock))
          (num-users (r:run sock (r:r (:count (:table "users")))))
          (num-boards (r:run sock (r:r (:count (:table "boards")))))
          (num-notes (r:run sock (r:r (:count (:table "notes")))))
          (num-personas (r:run sock (r:r (:count (:table "personas")))))
          (num-personas-wo-rsa (r:run sock (r:r (:count
                                                  (:filter (:table "personas")
                                                    (r:fn (p)
                                                      (:== (:default
                                                             (:attr p "pubkey")
                                                             nil)
                                                           nil))))))))
    (r:disconnect sock)
    (finish future (list :num-users num-users
                         :num-boards num-boards
                         :num-notes num-notes
                         :num-personas num-personas
                         :num-personas-wo-rsa num-personas-wo-rsa))))


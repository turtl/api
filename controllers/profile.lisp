(in-package :tagit)

(defroute (:get "/api/profiles/users/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (let ((user-id (car args)))
      (unless (string= (user-id req) user-id)
        (error 'insufficient-privileges :msg "You are trying to access another user's projects. For shame."))
      (alet* ((projects (get-user-projects user-id t))
              (settings nil)
              (response (make-hash-table :test #'equal)))
        (setf (gethash response "projects") projects
              (gethash response "settings") settings)
        (send-json res response)))))

(defun mock ()
  (let ((res (make-instance 'wookie:response)))
    (catch-errors (res)
      (alet* ((projects (get-user-projects "517b193a3dc42c1b2c000002" t))
              (response (make-hash-table :test #'equal)))
        (format t "projects: ~a~%" projects)
        (setf (gethash response "lololol") nil)
        (send-json res response)))))

(as:with-event-loop () (mock))

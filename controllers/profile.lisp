(in-package :tagit)

(defroute (:get "/api/profiles/users/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (let ((user-id (car args)))
      (unless (string= (user-id req) user-id)
        (error 'insufficient-privileges :msg "You are trying to access another user's boards. For shame."))
      (alet* ((boards (get-user-boards user-id :get-notes t :get-personas t))
              (user-data (get-user-data user-id))
              (response (make-hash-table :test #'equal)))
        (setf (gethash "boards" response) boards
              (gethash "user" response) user-data)
        (send-json res response)))))


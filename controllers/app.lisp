(in-package :tagit)

(defun user-id (request)
  "Grab a user id from a request."
  (getf (request-data request) :id))

(add-hook :pre-route
  (lambda (req res)
    (declare (ignore res))
    (let ((future (make-future)))
      ;; TODO: check user auth, finish future when it checks out =]
      (let ((user-info '(:id "516b9c503dc42c17a4000003")))
        (setf (request-data req) user-info)
        (finish future))
      future)))


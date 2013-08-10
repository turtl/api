(in-package :tagit)

;; TODO: pretend this is fucking built and get on with the persona changes,
;; dufus
(defun send-email (to subject body)
  "Send an email. Returns a future that finishes when the operation is done"
  (let ((future (make-future)))
    (as:delay (lambda () (finish future t)) :time .3)
    future))


(in-package :tagit)

(define-condition tagit-error (simple-error)
  ((msg :accessor error-msg :initarg :msg :initform "")
   (code :accessor error-code :initarg :code :initform 500))
  (:documentation "A general tagit error."))

(define-condition validation-failed (tagit-error)
  ((code :initform 400)))

(define-condition insufficient-privileges (tagit-error)
  ((code :initform 403)))

(define-condition not-found (tagit-error)
  ((code :initform 404)))

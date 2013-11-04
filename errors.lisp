(in-package :turtl)

(define-condition turtl-error (simple-error)
  ((msg :accessor error-msg :initarg :msg :initform "")
   (code :accessor error-code :initarg :code :initform 500))
  (:report (lambda (c s) (format s "~a" (error-msg c))))
  (:documentation "A general turtl error."))

(define-condition turtl-error-wrapper (simple-error)
  ((error :accessor error-error :initarg :error :initform nil)
   (function :accessor error-function :initarg :function :initform nil))
  (:report (lambda (c s) (format s "Turtl wrapped error: ~a: ~a" (error-function c) (error-error c))))
  (:documentation "Used to wrap caught errors and provide more debugging information about them."))

(define-condition validation-failed (turtl-error)
  ((code :initform 400)))

(define-condition auth-failed (turtl-error)
  ((code :initform 401)))

(define-condition insufficient-privileges (turtl-error)
  ((code :initform 403)))

(define-condition not-found (turtl-error)
  ((code :initform 404)))

(define-condition item-already-exists (turtl-error)
  ((code :initform 403)))

(define-condition server-error (turtl-error)
  ((code :initform 500)))


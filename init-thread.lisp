;;; This file holds a really simple wrapper around the tagit app to run it in a
;;; background thread so it can be updated whil running its event loop (which
;;; blocks the main thread, stopping swank from receiving commands).

(defpackage :tagit-thread
  (:use :cl)
  (:export :start
           :stop))
(in-package :tagit-thread)

(defvar *proc* nil
  "Holds the tagit background thread.")

(defun start (&key bind port)
  (when *proc*
    (format t "Tagit process exists. Try stopping with (tagit-background:stop)~%")
    (return-from start nil))
  (setf *proc* (bt:make-thread
                 (lambda ()
                   (tagit:start :bind bind :port port)
                   (format t "Tagit background thread finished.~%"))
                 :name "tagit"))
  *proc*)

(defun stop ()
  (when *proc*
    (bt:destroy-thread *proc*))
  (setf *proc* nil))


;;; This file holds a really simple wrapper around the turtl app to run it in a
;;; background thread so it can be updated whil running its event loop (which
;;; blocks the main thread, stopping swank from receiving commands).

(defpackage :turtl-thread
  (:use :cl)
  (:export :start
           :stop))
(in-package :turtl-thread)

(defvar *proc* nil
  "Holds the turtl background thread.")

(defun start (&key bind port)
  (when *proc*
    (format t "Turtl process exists. Try stopping with (turtl-thread:stop)~%")
    (return-from start nil))
  (setf *proc* (bt:make-thread
                 (lambda ()
                   (turtl:start :bind bind :port port)
                   (format t "Turtl background thread finished.~%")
                   (setf *proc* nil))
                 :name "turtl"))
  *proc*)

(defun stop ()
  (when *proc*
    (bt:destroy-thread *proc*))
  (setf *proc* nil))


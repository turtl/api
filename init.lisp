(in-package :tagit)

(setf cl-who:*attribute-quote-char* #\"
      (cl-who:html-mode) :html5
      cl-rethinkdb:*sequence-type* :list
      cl-rethinkdb:*object-type* :hash)

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun start (&key bind (port 81))
  ;; setup the wookie log
  (setf *log-level* :notice)

  ;; load/cache all the views
  (load-views)

  ;; start the server
  (let ((listener (make-instance 'listener :bind bind :port port)))
    (as:start-event-loop
      (lambda ()
        (start-server listener))
      :catch-app-errors nil)))


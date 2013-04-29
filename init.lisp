(in-package :tagit)

(setf cl-who:*attribute-quote-char* #\"
      (cl-who:html-mode) :html5
      cl-rethinkdb:*sequence-type* :list
      cl-rethinkdb:*object-type* :hash)

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun error-handler (err)
  (unless (typep err 'as:tcp-info)
    (format t "(tagit) error: ~a~%" err)))

(defun start (&key bind (port 81))
  ;; setup the wookie log
  (setf *log-level* :notice)

  (setf *error-handler* 'error-handler)

  ;; load/cache all the views
  (load-views)

  ;; start the server
  (let ((listener (make-instance 'listener :bind bind :port port)))
    (as:with-event-loop (:catch-app-errors t)
      (let ((server (start-server listener)))
        (as:signal-handler 2
          (lambda (sig)
            (declare (ignore sig))
            (as:close-tcp-server server)))))))


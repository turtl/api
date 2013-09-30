(in-package :turtl)

(setf cl-who:*attribute-quote-char* #\"
      (cl-who:html-mode) :html5
      cl-rethinkdb:*sequence-type* :list
      cl-rethinkdb:*object-type* :hash)

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun error-handler (err socket)
  (let* ((socket-data (as:socket-data socket))
         (response (getf socket-data :response)))
    (unless (typep err 'as:tcp-info)
      (when (and response (not (response-finished-p response)))
        (send-response response :status 500 :body "There was an error processing your request."))
      (format t "(turtl) UNcaught error: ~a~%" err))))

(defun start (&key bind (port 81))
  (setf *error-handler* 'error-handler)

  ;; load/cache all the views
  (load-views)

  ;; write our PID file (if *pid-file* is set)
  (when *pid-file*
    (with-open-file (s *pid-file*
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
      (format s "~a" (get-current-pid))))

  ;; start the server
  (unwind-protect
    (as:with-event-loop (:catch-app-errors nil)
      (let* ((listener (make-instance 'listener :bind bind :port port))
             (server (start-server listener)))
        (cleanup)
        (as:signal-handler 2
          (lambda (sig)
            (declare (ignore sig))
            (as:free-signal-handler 2)
            (as:close-tcp-server server)
            (as:exit-event-loop)))))
    (when *pid-file*
      (delete-file *pid-file*))))
  

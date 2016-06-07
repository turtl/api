(in-package :turtl)

;; do some general third-party lib setup
(setf cl-rethinkdb:*sequence-type* :list
      cl-rethinkdb:*object-type* :hash)

(defun error-handler (err &optional socket)
  "Main app error handler. Shouldn't be called all that often since every inch
   of our models/controllers are covered in catchers, but accidents do happen."
  (vom:debug "turtl error: ~a" err)
  (unless (and (typep err 'as:event-info)
               (not (typep err 'as:event-error)))
    (when (and socket (typep socket 'as:socket))
      (let* ((socket-data (as:socket-data socket))
             (request (getf socket-data :request))
             (response (getf socket-data :response)))
        ;; if we've got a response object and an error hasn't been sent yet, send
        ;; one. this will fix 99.99% of client hanging. the other 0.01% has yet to
        ;; be discovered.
        (when (and response
                   (not (response-finished-p response))
                   (not (as:socket-closed-p (request-socket request)))
                   (not (typep err 'auth-failed)))
          (let ((body (format nil "There was an error processing your request~a"
                              (if *display-errors*
                                  (format nil ": ~a" err)
                                  "."))))
            (send-response response :status 500 :body body)))))
    ;; let the guy looking at the logs see.
    (unless (subtypep (type-of err) 'as:streamish-eof)
      (vom:error "listener: ~a" err))))

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun start (&key (bind nil bind-supplied-p) (port nil port-supplied-p) custom-setup)
  "Start the Turtl app."
  ;; if no port was given, use the default from the config (if it exists)
  (when (and (not bind-supplied-p)
             (boundp '*server-bind*))
    (setf bind *server-bind*))
  (when (and (not port-supplied-p)
             (boundp '*server-port*))
    (setf port *server-port*))
  ;; write our PID file (if *pid-file* is set)
  (when *pid-file*
    (with-open-file (s *pid-file*
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
      (format s "~a" (get-current-pid))))

  ;; start the server
  (unwind-protect
    (let ((blackbird:*debug-on-error* (not *production-error-handling*))
          (wookie-config:*debug-on-error* (not *production-error-handling*)))
      (as:with-event-loop (:catch-app-errors (and *production-error-handling*
                                                  'error-handler))
        ;; set up the database schema
        (vom:info "Applying DB schema...")
        ;; call our setup function if we have one
        (when custom-setup (funcall custom-setup))
        (catcher
          (alet* ((report-main (apply-db-schema *db-schema*))
                  (report report-main))
            (vom:info "Schema applied: ~s" report)
            (let* ((listener (make-instance 'listener
                                            :bind bind
                                            :port port
                                            :event-cb 'error-handler))
                   (server (start-server listener))
                   (sighandler (lambda (sig)
                                 (declare (ignore sig))
                                 (vom:notice "Stopping turtl")
                                 (as:clear-signal-handlers)
                                 (as:close-tcp-server server)
                                 (as:exit-event-loop))))
              (cleanup)
              (as:signal-handler as:+sigint+ sighandler)
              (as:signal-handler as:+sigterm+ sighandler)))
          (error (e) (vom:error "Error initializing: ~a" e)))))
    (when *pid-file*
      (delete-file *pid-file*))))


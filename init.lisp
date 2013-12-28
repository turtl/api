(in-package :turtl)

;; do some general third-party lib setup
(setf cl-who:*attribute-quote-char* #\"
      (cl-who:html-mode) :html5
      cl-rethinkdb:*sequence-type* :list
      cl-rethinkdb:*object-type* :hash)

(defun error-handler (err socket)
  "Main app error handler. Shouldn't be called all that often since every inch
   of our models/controllers are covered in future-handler-case, but accidents
   do happen."
  (unless (typep err 'as:tcp-info)
    (when (and socket (typep socket 'as:socket))
      (let* ((socket-data (as:socket-data socket))
           (response (getf socket-data :response)))
        ;; if we've got a response object and an error hasn't been sent yet, send
        ;; one. this will fix 99.99% of client hanging. the other 0.01% has yet to
        ;; be discovered.
        (when (and response
                   (not (response-finished-p response))
                   (not (typep err 'auth-failed)))
          (let ((body (format nil "There was an error processing your request~a"
                              (if *display-errors*
                                  (format nil ": ~a" err)
                                  "."))))
            (send-response response :status 500 :body body)))))
    ;; let the guy looking at the logs see.
    (log:error "UNcaught error: ~a" err)))

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun start (&key bind (port 81))
  "Start the Turtl app."
  (setf *error-handler* 'error-handler)

  ;; load/cache all the views
  (when *enable-webapp*
    (load-views))

  ;; write our PID file (if *pid-file* is set)
  (when *pid-file*
    (with-open-file (s *pid-file*
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
      (format s "~a" (get-current-pid))))

  ;; start the server
  (unwind-protect
    (as:with-event-loop (:catch-app-errors t)
      ;; set up the database schema
      (log:info "Applying DB schema...")
      (future-handler-case
        (alet ((report (apply-db-schema *db-schema*)))
          (log:info "Schema applied: ~s" report)
          (let* ((listener (make-instance 'listener :bind bind :port port))
                 (server (start-server listener)))
            (cleanup)
            (as:signal-handler 2
              (lambda (sig)
                (declare (ignore sig))
                (as:free-signal-handler 2)
                (as:close-tcp-server server)
                (as:exit-event-loop)))))
        (t (e) (log:error "Error initializing: ~a" e))))
    (when *pid-file*
      (delete-file *pid-file*))))
  

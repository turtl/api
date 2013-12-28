(in-package :turtl)

(defmacro do-cleanup-action (action &rest args)
  "Wraps running of cleanup functions in an error handler."
  `(future-handler-case
     (,action ,@args)
     (t (e)
       (log:error "cleanup: Error running ~a: ~a" ',action e))))
  
(defun cleanup (&key (poll 60))
  "Run various cleanup/maintenance tasks. Specify :poll (in seconds) to do
   cleanup every N seconds."
  (do-cleanup-action cleanup-challenges)
  (do-cleanup-action cleanup-invites)
  (do-cleanup-action cleanup-sync)
  (as:delay 'cleanup :time poll))


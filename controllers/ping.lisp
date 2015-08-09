(in-package :turtl)

(route (:get "/ping") (req res)
  "Send a pong"
  (alet* ((immediate (not (zerop (varint (get-var req "immediate") 0))))
          (delay (if immediate 0 60)))
    (as:with-delay (delay)
      (block errexit
        (handler-bind ((as:socket-closed
                         (lambda (e)
                           (vom:warn "ping: socket closed: ~a" e)
                           (return-from errexit))))
          (send-json res "pong"))))))


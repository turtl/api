(ql:quickload :turtl)
(vom:config t :info)
(vom:config :cl-rethinkdb :warn)
(let ((blackbird:*debug-on-error* t)
      (wookie-config:*debug-on-error* t))
  (turtl:start :port 8182))


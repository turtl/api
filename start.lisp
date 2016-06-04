(ql:quickload :turtl)
(setf vom:*time-formatter* (lambda () (turtl::get-timestamp)))
(vom:config t :info)
(vom:config :cl-rethinkdb :warn)
(vom:config :turtl :debug)
(turtl:start :port 8181)


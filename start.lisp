(ql:quickload :turtl)
(vom:config t :info)
(vom:config :cl-rethinkdb :warn)
(vom:config :turtl :debug)
(turtl:start :port 8181)


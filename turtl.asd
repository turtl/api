(asdf:defsystem turtl
  :author "Lyon Bros. Enterprises, LLC <info@lyonbros.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An encrypted note/bookmark/data organizer."
  :depends-on (#:cl-async-future
               #:cl-async
               #:wookie
               #:cl-rethinkdb
               #:cl-ppcre
               #:cl-who
               #:cl-fad
               #:yason
               #:cl-mongo-id
               #:cl-base64
               #:drakma-async
               #:local-time
               #:ironclad
               #:secure-random
               #:bordeaux-threads)
  :components
  ((:file "package")
   (:module lib
    :depends-on ("package")
    :serial t
    :components
    ((:file "util")
     (:file "validation")
     (:file "crypto")))
   (:file "config/config" :depends-on (lib))
   (:file "crypto" :depends-on (lib "config/config"))
   (:file "errors" :depends-on (lib "config/config" "crypto"))
   (:file "template" :depends-on (lib "crypto" "errors"))
   (:file "cron" :depends-on (lib "config/config"))
   (:file "init" :depends-on ("template" "crypto" "errors" "cron"))
   (:file "routes" :depends-on ("init" "crypto" "errors"))
   (:file "init-thread" :depends-on ("init" "routes"))))


(asdf:defsystem turtl
  :author "Lyon Bros. Enterprises, LLC <info@lyonbros.com>"
  :license "AGPLv3"
  :version "0.1.0"
  :description "An encrypted note/bookmark/data organizer."
  :depends-on (#:blackbird
               #:cl-async
               #:wookie
               #:cl-rethinkdb
               #:cl-hash-util
               #:cl-ppcre
               #:flexi-streams
               #:cl-fad
               #:jonathan
               #:cl-mongo-id
               #:cl-base64
               #:drakma-async
               #:local-time
               #:ironclad
               #:secure-random
               #:bordeaux-threads
               #:trivial-backtrace
               #:xmls
               #:vom)
  :components
  ((:file "package")
   (:file "config/config" :depends-on ("package"))
   (:file "config/schema" :depends-on ("config/config"))
   (:module lib
    :depends-on ("package" "config/config" "config/schema")
    :serial t
    :components
    ((:file "util")
     (:file "validation")
     (:file "crypto")
     (:file "s3")))
   (:file "crypto" :depends-on (lib "config/config"))
   (:file "errors" :depends-on (lib "config/config" "crypto"))
   (:file "cron" :depends-on (lib "config/config"))
   (:module models
    :depends-on (lib "errors" "crypto" "package" "config/schema")
    :serial t
    :components
    ((:file "schema")
     (:file "users")
     (:file "keychain")
     (:file "email")
     (:file "personas")
     (:file "notes")
     (:file "boards")
     (:file "invites")
     (:file "profile")
     (:file "sync")
     (:file "log")
     (:file "admin")
     (:file "analytics")
     (:file "feedback")
     (:file "promo")))
   (:file "init" :depends-on (lib "crypto" "errors" "cron" "config/schema" models))
   (:module controllers
    :depends-on ("init" "errors" lib models "package")
    :serial t
    :components
    ((:file "admin")
     (:file "app")
     (:file "sharing")
     (:file "notes")
     (:file "personas")
     (:file "sync")
     (:file "users")
     (:file "feedback")
     (:file "log")))
   (:file "routes" :depends-on (lib "init" controllers "crypto" "errors"))
   (:file "init-thread" :depends-on (lib "init" "routes"))))


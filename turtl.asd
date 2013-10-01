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
               #:flexi-streams
               #:cl-who
               #:cl-fad
               #:yason
               #:cl-mongo-id
               #:cl-base64
               #:drakma-async
               #:local-time
               #:ironclad
               #:secure-random
               #:bordeaux-threads
               #:xmls)
  :components
  ((:file "package")
   (:module lib
    :depends-on ("package")
    :serial t
    :components
    ((:file "util")
     (:file "validation")
     (:file "crypto")
     (:file "s3")))
   (:file "config/config" :depends-on (lib))
   (:file "crypto" :depends-on (lib "config/config"))
   (:file "errors" :depends-on (lib "config/config" "crypto"))
   (:file "template" :depends-on (lib "crypto" "errors"))
   (:file "cron" :depends-on (lib "config/config"))
   (:module models
    :depends-on ("errors" "crypto" lib "package")
	:serial t
	:components
	((:file "users")
	 (:file "challenges")
	 (:file "email")
	 (:file "personas")
	 (:file "notes")
	 (:file "boards")
	 (:file "messages")
	 (:file "files")
	 (:file "invites")
	 (:file "sync")
	 (:file "admin")
	 (:file "analytics")))
   (:file "init" :depends-on ("template" "crypto" "errors" "cron"))
   (:module controllers
    :depends-on ("errors" lib models "package")
	:serial t
	:components
	((:file "admin")
	 (:file "app")
	 (:file "boards")
	 (:file "files")
	 (:file "invites")
	 (:file "messages")
	 (:file "notes")
	 (:file "personas")
	 (:file "profile")
	 (:file "sync")
	 (:file "users")))
   (:file "routes" :depends-on ("init" controllers "crypto" "errors"))
   (:file "init-thread" :depends-on ("init" "routes"))))


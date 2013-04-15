(asdf:defsystem tagit
  :author "Lyon Bros. Enterprises, LLC <info@lyonbros.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An encrypted note/bookmark/data organizer."
  :depends-on (#:cl-async-future #:cl-async #:wookie #:cl-rethinkdb #:cl-who #:cl-fad #:yason #:cl-mongo-id)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("config"))
   (:file "template" :depends-on ("util"))
   (:file "init" :depends-on ("template"))
   (:file "routes" :depends-on ("init"))))


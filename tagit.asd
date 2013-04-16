(asdf:defsystem tagit
  :author "Lyon Bros. Enterprises, LLC <info@lyonbros.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An encrypted note/bookmark/data organizer."
  :depends-on (#:cl-async-future #:cl-async #:wookie #:cl-rethinkdb #:cl-ppcre #:cl-who #:cl-fad #:yason #:cl-mongo-id)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("config"))
   (:file "errors" :depends-on ("util" "config"))
   (:file "template" :depends-on ("util" "errors"))
   (:file "init" :depends-on ("template" "errors"))
   (:file "routes" :depends-on ("init" "errors"))))


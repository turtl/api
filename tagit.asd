(asdf:defsystem tagit
  :author "Lyon Bros. Enterprises, LLC <info@lyonbros.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An encrypted note/bookmark/data organizer."
  :depends-on (#:cl-async-future #:cl-async #:wookie #:cl-rethinkdb #:cl-ppcre #:cl-who #:cl-fad #:yason #:cl-mongo-id #:cl-base64 #:local-time #:ironclad)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("config"))
   (:file "crypto" :depends-on ("util" "config"))
   (:file "errors" :depends-on ("util" "config" "crypto"))
   (:file "template" :depends-on ("util" "crypto" "errors"))
   (:file "init" :depends-on ("template" "crypto" "errors"))
   (:file "routes" :depends-on ("init" "crypto" "errors"))))


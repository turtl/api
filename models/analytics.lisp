(in-package :turtl)

(defafun track (future) (event &optional data request)
  "Tracks an analytics event."
  (if (getf *analytics* :enabled)
      (alet* ((data (if request
                        (append data
                                (list :uid (user-id request)
                                      :client (get-client request)))
                        data))
              (record `(("id" . ,(string-downcase (mongoid:oid-str (mongoid:oid))))
                        ("event" . ,event)
                        ("data" . ,(convert-plist-hash data :convert-nulls t))))
              (sock (db-sock "analytics"))
              (query (r:r (:insert (:table "events") record)))
              (nil (r:run sock query)))
        (format t "anal: ~s~%" record)
        (finish future t))
      (finish future t)))


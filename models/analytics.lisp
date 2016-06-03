(in-package :turtl)

(defun %track (event &optional data request)
  "Track an analytics event."
  (unless (getf *analytics* :enabled)
    (return-from %track))
  (let* ((data (if request
                   (append data
                           (list :uid (user-id request)
                                 :client (get-client request)))
                   data))
         (record (hash ("event" event)
                       ("data" (if (hash-table-p data)
                                   data
                                   (convert-plist-hash data :convert-nulls t))))))
    (vom:notice "*a :: ~a" (to-json record))))

(adefun track (event &optional data request)
  (%track event data request))

(defafun track_ (future) (event &optional data request)
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
              (sock (db-sock :db "analytics"))
              (query (r:r (:insert (:table "events") record)))
              (nil (r:run sock query)))
        (r:disconnect sock)
        (finish future t))
      (finish future t)))


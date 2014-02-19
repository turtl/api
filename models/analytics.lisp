(in-package :turtl)

(defafun track (future) (event &optional data)
  "Tracks an analytics event."
  (if (getf *analytics* :enabled)
      (alet* ((sock (db-sock "analytics"))
              (query (r:r (:insert
                            (:table "events")
                            `(("event" . ,event)
                              ("data" . ,data)))))
              (nil (r:run sock query)))
        (finish future t))
      (finish future t)))


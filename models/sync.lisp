(in-package :tagit)

(defafun sync-notes (future) (user-id sync-time)
  "Grab all notes for this user that have changed."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:table "notes")
                     (r:fn (note)
                       (:&&
                         (:contains (:map
                                      (:get-all (:table "projects")
                                                user-id
                                                "user_id")
                                      (r:fn (project) (:attr project "id")))
                                    (:attr note "project_id"))
                         (:> (:default (:attr note "mod") 0)
                             sync-time))))))
          (cur (r:run sock query)))
    (alet ((arr (r:to-array sock cur)))
      (r:disconnect sock)
      (finish future arr))))


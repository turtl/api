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
                                      (:get-all (:table "boards")
                                                user-id
                                                :index "user_id")
                                      (r:fn (board) (:attr board "id")))
                                    (:attr note "board_id"))
                         (:> (:default (:attr note "mod") 0)
                             sync-time))))))
          (cursor (r:run sock query)))
    (alet ((arr (r:to-array sock cursor)))
      (wait-for (r:stop sock cursor)
        (r:disconnect sock))
      (finish future arr))))


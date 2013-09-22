(in-package :turtl)

(defafun sync-user (future) (user-id sync-time)
  "Grab any changed user data."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "users") user-id)))
          (user (r:run sock query)))
    (r:disconnect sock)
    (if (< sync-time (or (gethash "mod" user) 99999999))
        (finish future user)
        (finish future nil))))

(defafun sync-user-personas (future) (user-id sync-time)
  "Grab any changed personas."
  (alet* ((sock (db-sock))
          (query (r:r 
                   (:filter
                     (:get-all
                       (:table "personas")
                       user-id
                       :index "user_id")
                     (r:fn (persona)
                       (:&& (:> (:default (:attr persona "mod") 0)
                                sync-time))))))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future personas)))

(defafun sync-user-boards (future) (user-id sync-time &key get-persona-boards get-personas)
  "Grab all changed boards for a user."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:get-all
                       (:table "boards")
                       user-id
                       :index "user_id")
                     (r:fn (board)
                       (:&& (:== (:attr board "user_id") user-id)
                            (:> (:default (:attr board "mod") 0)
                                sync-time))))))
          (cursor (r:run sock query))
          (boards (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (alet* ((persona-boards (if get-persona-boards
                                (user-personas-map
                                  user-id
                                  (lambda (pid)
                                    (sync-persona-boards pid sync-time))
                                  :flatten t)
                                #()))
            (all-boards (cl-async-util:append-array boards persona-boards))
            (boards-populated (populate-boards-data all-boards
                                                    :get-personas get-personas)))
      (finish future boards-populated))))

(defafun sync-persona-boards (future) (persona-id sync-time)
  "Grab all a persona's changed boards (shared)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:map
                     (:filter
                       (:get-all
                         (:table "boards_personas_link")
                         persona-id
                         :index "to")
                       (r:fn (link)
                         (:&& (:<= sync-time (:default (:attr link "mod") 0))
                              (:~ (:has-fields link "invite")))))
                     (r:fn (link)
                       (:get (:table "boards") (:attr link "board_id"))))))
          (boards (r:run sock query))
          (boards (populate-boards-data (coerce boards 'simple-vector) :set-shared t)))
    (r:disconnect sock)
    (finish future boards)))

(defafun sync-user-notes (future) (user-id sync-time &key get-persona-notes)
  "Grab all notes for this user that have changed."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:attr
                       (:eq-join
                         (:get-all
                           (:table "boards")
                           user-id
                           :index "user_id")
                         "id"
                         (:table "notes")
                         :index "board_id")
                       "right")
                     (r:fn (note)
                       (:> (:default (:attr note "mod") 0) sync-time)))))
          (notes (r:run sock query))
          (notes (coerce notes 'simple-array)))
    (r:disconnect sock)
    (alet* ((persona-notes (if get-persona-notes
                               (user-personas-map
                                 user-id
                                 (lambda (pid) (sync-persona-notes pid sync-time))
                                 :flatten t)
                               #()))
            (all-notes (cl-async-util:append-array notes persona-notes)))
      (finish future all-notes))))

(defafun sync-persona-notes (future) (persona-id sync-time)
  "Grab all a persona's changed notes (shared)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:attr
                       (:eq-join
                         (:get-all
                           (:table "boards_personas_link")
                           persona-id
                           :index "to")
                         "board_id"
                         (:table "notes")
                         :index "board_id")
                       "right")
                     (r:fn (note)
                       (:<= sync-time (:default (:attr note "mod") 0))))))
          (notes (r:run sock query))
          (notes (coerce notes 'simple-array)))
    (r:disconnect sock)
    (finish future notes)))


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
                                  (lambda (pid) (sync-persona-boards pid sync-time))
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
                   ;; TODO: index
                   (:filter
                     (:table "boards")
                     (r:fn (board)
                       ;; pull out boards where privs has the persona-id, but
                       ;; it's NOT an invite
                       (:&& (:has-fields (:attr board "privs") persona-id)
                            (:~ (:has-fields (:attr (:attr board "privs") persona-id) "i"))
                            (:> (:default (:attr board "mod") 0)
                                sync-time))))))
          (cursor (r:run sock query))
          (boards (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future boards)))

(defafun sync-user-notes (future) (user-id sync-time &key get-persona-notes)
  "Grab all notes for this user that have changed."
  (alet* ((sock (db-sock))
          (query (r:r
                   ;; TODO: index
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
          (cursor (r:run sock query))
          (notes (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
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
                   ;; TODO: index
                   (:do
                     (r:fn (board-ids)
                       (:filter
                         (:table "notes")
                         (r:fn (note)
                           (:&&
                             (:contains board-ids (:attr note "board_id"))
                             (:> (:default (:attr note "mod") 0)
                                 sync-time)))))
                     (:coerce-to
                       (:map
                         (:filter
                           (:table "boards")
                           (r:fn (board)
                             ;; pull out boards where privs has the persona-id, but
                             ;; it's NOT an invite
                             (:&& (:has-fields (:attr board "privs") persona-id)
                                  (:~ (:has-fields (:attr (:attr board "privs") persona-id) "i")))))
                         (r:fn (board) (:attr board "id")))
                       "array"))))
          (cursor (r:run sock query))
          (notes (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future notes)))


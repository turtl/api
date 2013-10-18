(in-package :turtl)

(defafun add-sync-record (future) (user-id item-type item-id action &key rel-id)
  "Adds a record to the sync table describing a change to a specific object.
   Allows specifying a relation id (:rel-id) which can be used for quick lookups
   on sync items. This id is completely dependant on what model is calling this
   function. For instance, a boards_personas_link entry might use the board ID
   as the rel-id."
  ;; bomb out if bac action given (should never happen since this function is
  ;; only used internally, but accidents to happen)
  (unless (find action '("add" "update" "delete") :test #'string=)
    (signal-error future (make-instance 'server-error
                                        :msg (format nil "Bad sync record action: ~s~%" action)))
    (return-from add-sync-record))
  (let* ((sync-record (make-hash-table :test #'equal)))
    (add-id sync-record)
    (setf (gethash "user_id" sync-record) user-id
          (gethash "type" sync-record) item-type
          (gethash "item_id" sync-record) item-id
          (gethash "action" sync-record) action)
    ;; set our relation, if specified
    (when rel-id (setf (gethash "rel" sync-record) rel-id))
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "sync")
                          sync-record)))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future sync-record))))

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
          ;; TODO: fix, only sync when board <--> persona relationship changes
          ;; for instance, changing the name of a shared board won't sync to
          ;; sharee
          ;; TODO: index MORE
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
          (cursor (r:run sock query))
          (boards (r:to-array sock cursor))
          (boards (populate-boards-data (coerce boards 'simple-vector) :set-shared t)))
    (r:stop/disconnect sock cursor)
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
          (cursor (r:run sock query))
          (notes (r:to-array sock cursor))
          (notes (coerce notes 'simple-array)))
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
          (cursor (r:run sock query))
          (notes (r:to-array sock cursor))
          (notes (coerce notes 'simple-array)))
    (r:stop/disconnect sock cursor)
    (finish future notes)))


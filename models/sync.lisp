(in-package :turtl)

(defafun add-sync-record (future) (user-id item-type item-ids action &key client-id rel-id)
  "Adds a record to the sync table describing a change to a specific object.
   Allows specifying a relation id (:rel-id) which can be used for quick lookups
   on sync items. Returns the added sync records IDs as the first value and the
   sync records as the second."
  ;; bomb out if bac action given (should never happen since this function is
  ;; only used internally, but accidents to happen)
  (unless (find action '("add" "edit" "delete") :test #'string=)
    (signal-error future (make-instance 'server-error
                                        :msg (format nil "Bad sync record action: ~s~%" action)))
    (return-from add-sync-record))
  ;; allow batch-inserting (based on item-id)
  (let* ((item-ids (if (listp item-ids)
                       item-ids
                       (list item-ids)))
         (records nil))
    (dolist (item-id item-ids)
      (let* ((sync-record (make-hash-table :test #'equal)))
        (add-id sync-record)
        (setf (gethash "user_id" sync-record) user-id
              (gethash "type" sync-record) item-type
              (gethash "item_id" sync-record) item-id
              (gethash "action" sync-record) action)
        ;; set our relation, if specified
        (when rel-id (setf (gethash "rel" sync-record) rel-id))
        (when client-id (setf (gethash "cid" sync-record) client-id))
        (push sync-record records)))
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "sync")
                          records)))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future (mapcar (lambda (x) (gethash "id" x)) records)))))

(defafun link-sync-items (future) (sync-items link-table)
  "Given an array of items pulled from the `sync` table and a string table name
   to link the items against, populate the sync items with their linked counter
   parts (including the sync_id field for each sync item)."
  ;; split up the items by deleted (ie, nother to link against, so we return a
  ;; "fake" record with deleted=true) or present (in which can we grab the
  ;; present items from the link-table and return them with their sync-id
  ;; set). this gives us a completed picture of what's been changed and/or
  ;; deleted.
  (let ((deleted-items nil)
        (present-items nil))
    (loop for sync-item across sync-items do
      (if (string= (gethash "action" sync-item) "delete")
          ;; create a return-ready "deleted" record
          (let ((hash (make-hash-table :test #'equal)))
            (setf (gethash "id" hash) (gethash "item_id" sync-item)
                  (gethash "sync_id" hash) (gethash "id" sync-item)
                  (gethash "deleted" hash) t)
            (push hash deleted-items))
          ;; item is present, so save it (and its sync-id to pull out of the db
          ;; later)
          (push (cons (gethash "item_id" sync-item)
                      (gethash "id" sync-item))
                present-items)))
    (flet ((finish (items)
             (let* ((index (make-hash-table :test #'equal))
                    (synced-items nil))
               ;; index the present items by id
               (loop for item across items do
                 (setf (gethash (gethash "id" item) index) item))
               ;; for each item we believe to be present, create a new hash
               ;; record for it with the sync_id present
               (dolist (present present-items)
                 (let* ((item-id (car present))
                        (sync-id (cdr present))
                        (sync (gethash item-id index))
                        (sync-copy (make-hash-table :test #'equal)))
                   ;; sync could possibly be nil (if an item is edited and then
                   ;; deleted in the same sync call, then although the edit
                   ;; fools us into thinking the item is present, the delete
                   ;; actually removed it. in this case, it will also be in
                   ;; deleted-items and we don't need to bother tracking it).
                   (when sync
                     ;; shallow-copy the sync entrya (so we can have the same data with a
                     ;; different sync-id)
                     (loop for k being the hash-keys of sync
                           for v being the hash-values of sync do
                       (setf (gethash k sync-copy) v))
                     ;; save the sync_id
                     (setf (gethash "sync_id" sync-copy) sync-id)
                     (push sync-copy synced-items))))
               ;; return the array of items, sorted by sync_id ASC
               (finish future
                       (coerce (sort (append synced-items deleted-items)
                                     (lambda (a b)
                                       (string< (gethash "sync_id" a)
                                                (gethash "sync_id" b))))
                               'vector)))))
      ;; if we have no items to link, just finish with a blank array, otherwise
      ;; pull out our items and finish with the list
      (if (zerop (length present-items))
          (finish #())
          (alet* ((sock (db-sock))
                  (query (r:r
                           (:get-all
                             (:table link-table)
                             (mapcar (lambda (x) (car x)) present-items))))
                  (cursor (r:run sock query))
                  (items (r:to-array sock cursor)))
            (r:stop/disconnect sock cursor)
            (finish items))))))
  
  
(defafun sync-user-items (future) (user-id sync-id type link-table)
  "Generic query function to grab a user's sync items of a specific type. It
   grabs the items themselves from link-table, and injects the sync_id of the
   sync that the items is being pulled from."
  ;; first, grab all sync items for this user/type
  (alet* ((sock (db-sock))
          (query (r:r
                   (:between
                        (:table "sync")
                        (list user-id type sync-id)
                        (list user-id type "z")
                        :index (db-index "sync" "user_search")
                        :left-bound "open")))
          (cursor (r:run sock query))
          (sync-items (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (alet ((sync (link-sync-items sync-items link-table)))
      (finish future sync))))

(defafun sync-user (future) (user-id sync-id)
  "Grab any changed user data."
  (alet* ((sync (sync-user-items user-id sync-id "user" "users")))
    (finish future sync)))

(defafun sync-user-keychain (future) (user-id sync-time)
  "Grab all changed keychain entries."
  (alet* ((sync (sync-user-items user-id sync-id "keychain" "keychain")))
    (finish future sync)))

(defafun sync-user-personas (future) (user-id sync-time)
  "Grab any changed personas."
  (alet* ((sync (sync-user-items user-id sync-id "persona" "personas")))
    (finish future sync)))

(defafun sync-user-boards (future) (user-id sync-id &key get-personas)
  "Grab all changed boards for a user."
  (alet* ((board-ids (get-all-user-board-ids user-id))
          (sock (db-sock))
          (query (r:r
                   (:filter
                     (:get-all
                       (:table "sync")
                       (coerce board-ids 'list)
                       :index (db-index "sync" "item_id"))
                     (r:fn (s)
                       (:&& (:< sync-id (:attr s "id"))
                            )))))
          (cursor (r:run sock query))
          (sync-items (r:to-array sock cursor)))
    (format t "boards: ~a~%" (length board-ids))
    (format t "sync: ~a~%" sync-items)
    (r:stop/disconnect sock cursor)
    (alet* ((boards (link-sync-items sync-items "boards"))
            (boards (populate-boards-data boards
                                          :get-personas get-personas)))
      (finish future boards))))

(as:with-event-loop (:catch-app-errors t)
  (future-handler-case
    (alet* ((sync (sync-user-boards "5241e230735ca4892c000002" "526491c9735ca4190c000007" :get-personas nil)))
      (format t "sync: ~s~%" sync))
    (t (e) (format t "err: ~a~%" e))))


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
                         :index (db-index "boards_personas_link" "to"))
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
                           :index (db-index "boards" "user_id"))
                         "id"
                         (:table "notes")
                         :index (db-index "notes" "board_id"))
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
                           :index (db-index "boards_personas_link" "to"))
                         "board_id"
                         (:table "notes")
                         :index (db-index "notes" "board_id"))
                       "right")
                     (r:fn (note)
                       (:<= sync-time (:default (:attr note "mod") 0))))))
          (cursor (r:run sock query))
          (notes (r:to-array sock cursor))
          (notes (coerce notes 'simple-array)))
    (r:stop/disconnect sock cursor)
    (finish future notes)))

(defafun sync-user_ (future) (user-id sync-time)
  "Grab any changed user data."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "users") user-id)))
          (user (r:run sock query)))
    (r:disconnect sock)
    (if (< sync-time (or (gethash "mod" user) 99999999))
        (finish future user)
        (finish future nil))))

(defafun sync-user-keychain_ (future) (user-id sync-time)
  "Grab all changed keychain entries."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:get-all
                       (:table "keychain")
                       user-id
                       :index (db-index "keychain" "user_id"))
                     (r:fn (entry)
                       (:&& (:> (:default (:attr entry "mod") 0)
                                sync-time))))))
          (cursor (r:run sock query))
          (keychain (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future keychain)))

(defafun sync-user-personas_ (future) (user-id sync-time)
  "Grab any changed personas."
  (alet* ((sock (db-sock))
          (query (r:r 
                   (:filter
                     (:get-all
                       (:table "personas")
                       user-id
                       :index (db-index "personas" "user_id"))
                     (r:fn (persona)
                       (:&& (:> (:default (:attr persona "mod") 0)
                                sync-time))))))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future personas)))

(defafun sync-user-boards_ (future) (user-id sync-time &key get-persona-boards get-personas)
  "Grab all changed boards for a user."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:get-all
                       (:table "boards")
                       user-id
                       :index (db-index "boards" "user_id"))
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

(defafun sync-persona-boards_ (future) (persona-id sync-time)
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
                         :index (db-index "boards_personas_link" "to"))
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

(defafun sync-user-notes_ (future) (user-id sync-time &key get-persona-notes)
  "Grab all notes for this user that have changed."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:attr
                       (:eq-join
                         (:get-all
                           (:table "boards")
                           user-id
                           :index (db-index "boards" "user_id"))
                         "id"
                         (:table "notes")
                         :index (db-index "notes" "board_id"))
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

(defafun sync-persona-notes_ (future) (persona-id sync-time)
  "Grab all a persona's changed notes (shared)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:attr
                       (:eq-join
                         (:get-all
                           (:table "boards_personas_link")
                           persona-id
                           :index (db-index "boards_personas_link" "to"))
                         "board_id"
                         (:table "notes")
                         :index (db-index "notes" "board_id"))
                       "right")
                     (r:fn (note)
                       (:<= sync-time (:default (:attr note "mod") 0))))))
          (cursor (r:run sock query))
          (notes (r:to-array sock cursor))
          (notes (coerce notes 'simple-array)))
    (r:stop/disconnect sock cursor)
    (finish future notes)))


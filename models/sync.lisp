(in-package :turtl)

(defafun get-latest-sync-id (future) ()
  "Retrieve the last sync-id from the sync table. This gives a newly-populated
   client a reference point to run syncs against."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:attr
                     (:limit
                       (:order-by
                         (:table "sync")
                         :index (:desc "id"))
                       1)
                     "id")))
          (cursor  (r:run sock query))
          (sync-item (r:to-array sock cursor))
          (sync-item (coerce sync-item 'list)))
    (r:stop/disconnect sock cursor)
    (finish future (car sync-item))))

(defun make-sync-record (user-id item-type item-id action &key client-id rel-ids fields no-auto-add-user)
  "Creates a sync hash record from the given params."
  (let* ((sync-record (make-hash-table :test #'equal)))
    (add-id sync-record)
    (setf (gethash "user_id" sync-record) user-id
          (gethash "type" sync-record) item-type
          (gethash "item_id" sync-record) item-id
          (gethash "action" sync-record) action)
    ;; the originating user should always be in the relations
    (unless no-auto-add-user
      (if (listp rel-ids)
          (push user-id rel-ids)
          (setf rel-ids (concatenate 'vector rel-ids (vector user-id)))))
    (setf (gethash "rel" sync-record) (remove-duplicates rel-ids :test #'string=))
    ;; can store the client id (cid) of a newly-created object
    (when client-id (setf (gethash "cid" sync-record) client-id))
    ;; can be used to specify the public fields changed in an edit
    (when (and fields (listp fields)) (setf (gethash "fields" sync-record) fields))
    sync-record))

(defun convert-to-sync (item type &key (action "add"))
  "Take a piece of data (say, a note) and turn it into a sync item the app can
   understand. Very useful for pulling out extra data into a profile that didn't
   come through sync but we want to be available to the app.
   
   Defaults to an 'add' but can be specified via :action."
  (let ((rec (make-sync-record (gethash "user_id" item)
                               type
                               (gethash "id" item)
                               action)))
    (case (intern (string-upcase action) :keyword)
      (:delete
        (setf (gethash "data" rec) (hash ("id" (gethash "id" item))
                                         ("deleted" t))))
      (t
        (setf (gethash "data" rec) item)))
    rec))

(defafun insert-sync-records (future) (sync-records)
  "Insert one or more sync-records (list) objects (built with make-sync-record)
   into the sync table."
  (alet* ((sock (db-sock))
          (query (r:r (:insert
                        (:table "sync")
                        (if (zerop (length sync-records))
                            #()
                            sync-records))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future t)))

(adefun add-sync-record (user-id item-type item-id action &key sub-action client-id rel-ids fields no-auto-add-user)
  "Adds a record to the sync table describing a change to a specific object.
   Allows specifying relation ids (:rel-ids) which can be used for filtering on
   sync items. Returns the added sync records IDs as the first value and the
   full sync records as the second."
  (declare (ignore sub-action fields))
  ;; bomb out if bac action given (should never happen since this function is
  ;; only used internally, but accidents to happen)
  (unless (find action '("add" "edit" "delete" "share" "unshare") :test #'string=)
    (error 'server-error :msg (format nil "Bad sync record action: ~s~%" action)))
  (alet* ((sync-record (make-sync-record user-id item-type item-id action :client-id client-id :rel-ids rel-ids :no-auto-add-user no-auto-add-user))
          (nil (insert-sync-records (list sync-record))))
    (list (gethash "id" sync-record))))

(defafun link-sync-items (future) (sync-items link-table)
  "Given an array of items pulled from the `sync` table and a string table name
   to link the items against, populate the sync items with their linked counter
   parts (including the sync_id field for each sync item).

   Note that all functions that deal with syncing should call this function. It
   not only makes linking sync items to their data counterparts easier, it
   uses a standard format for everything."
  ;; split up the items by deleted (ie, can't link against it, so we return a
  ;; "fake" record with deleted=true) or present (in which can we grab the
  ;; present items from the link-table and return them (along with any sync
  ;; metadata set. this gives us a completed picture of what's been changed
  ;; and/or deleted.
  (let ((deleted-items nil)
        (present-items nil))
    (loop for sync-item across sync-items do
      ;; test if the item was deleted
      (if (string= (gethash "action" sync-item) "delete")
          ;; create a return-ready "deleted" record, complete with sync metadata
          (let ((item (hash ("id" (gethash "item_id" sync-item))
                            ("deleted" t))))
            (setf (gethash "data" sync-item) item)
            (push sync-item deleted-items))
          ;; item is present, so save it (and its sync-id to pull out of the db
          ;; later).
          (push sync-item present-items)))
    ;; define our finalizing function. this is needed because sometimes we'll
    ;; call out to the DB to pull out present items, sometimes we won't, and
    ;; since we're dealing with async, we define a function to handle both
    ;; instances
    (flet ((finish (items)
             (let* ((index (make-hash-table :test #'equal))
                    (synced-items nil))
               ;; index the present items by id
               (loop for item across items do
                 (setf (gethash (gethash "id" item) index) item))
               ;; for each item we believe to be present, create a new hash
               ;; record for it with the sync_id present
               (dolist (rec present-items)
                 (let* ((item (gethash (gethash "item_id" rec) index)))
                   ;; sync could possibly be nil (if an item is edited and then
                   ;; deleted in the same sync call, then although the edit
                   ;; fools us into thinking the item is present, the delete
                   ;; actually removed it. in this case, it will also be in
                   ;; deleted-items and we don't need to bother tracking it).
                   (when item
                     ;; create a sync record and save the object into it
                     (setf (gethash "data" rec) item))
                     (push rec synced-items)))
               ;; return the array of items, sorted by sync_id DESC
               (finish future
                       (coerce (sort (append synced-items deleted-items)
                                     (lambda (a b)
                                       (string> (gethash "id" a)
                                                (gethash "id" b))))
                               'vector)))))
      ;; if we have no items to link, just finish with a blank array, otherwise
      ;; pull out our items and finish with the list
      (if (zerop (length present-items))
          (finish #())
          (alet* ((sock (db-sock))
                  (query (r:r
                           (:get-all
                             (:table link-table)
                             (mapcar (lambda (x) (gethash "item_id" x)) present-items))))
                  (cursor (r:run sock query))
                  (items (r:to-array sock cursor)))
            (r:stop/disconnect sock cursor)
            (finish items))))))

(adefun sync-scan (user-id from-sync-id &key poll)
  "Given a user id, sync id, and item type, pull out all sync records *after*
   the given sync-id, where the `rel` field contains the given user-id, and the
   sync type matches the passed type. Links grabbed sync items against the given
   link-table.
   
   This is useful for boards/notes, because whenever they change (ie add a sync
   record) they also record (in the `rel` field) which users are affected by the
   change."
  (alet* ((sock (db-sock))
          (poll-timeout 30)
          (sock-poll (when poll (db-sock :timeout poll-timeout)))
          (query (r:r
                   (:between
                     (:table "sync")
                     (list user-id from-sync-id)
                     (list user-id (:maxval))
                     :index (db-index "sync" "scan_user")
                     :left-bound "open")))
          ;; wrap changes around the above query
          (query-poll (r:r (:changes query :squash 1))))
    ;; run the changes query, saving the return promise
    (let ((poll-promise (when poll (r:run sock-poll query-poll))))
      ;; run the non-changes query and grab the results
      (alet* ((cursor (r:run sock query))
              (sync-items (r:to-array sock cursor))
              (sync-size (length sync-items)))
        (r:stop/disconnect sock cursor)
        (cond ((and poll (zerop sync-size))
               ;; we got no items returned from the instant query, run the changes
               ;; query (aka wait on the above promise) and save the results as
               ;; they come in
               (alet* ((cursor-poll poll-promise)
                       (results nil)
                       (timer nil))
                 (as:with-delay ((+ poll-timeout 1))
                   (r:stop/disconnect sock-poll cursor-poll))
                 (chain
                   (r:each sock-poll cursor-poll
                     (lambda (rec)
                       (unless timer
                         (setf timer (as:with-delay (.1) (r:stop/disconnect sock-poll cursor-poll))))
                       (push rec results)))
                   (:catch (err)
                     (unless (typep err 'r:cursor-stopped)
                       (error err)))
                   (:finally
                     (r:stop/disconnect sock-poll cursor-poll)
                     (coerce (nreverse results) 'list)))))
              ((zerop sync-size)
               #())
              (t
               (when poll (r:disconnect sock-poll))
               sync-items))))))

(adefun sync-all (user-id last-sync-id &key poll)
  "Grab all of the sync records for the given user-id since last-sync-id, link
   them to their respective objects, and hand back the sorted (ASC) list of sync
   items."
  (alet* ((types (hash))
          (last-sync-id (or last-sync-id (r:r (:maxval))))
          (records (sync-scan user-id last-sync-id :poll poll)))
    ;; group our sync records by type so we can pull them out en-mass
    (loop for record across records
          for type = (gethash "type" record) do
      (push record (gethash type types)))
    ;; loop through our groups records and link the corresponding objects
    (let ((actions nil))
      (loop for type being the hash-keys of types
            for collection being the hash-values of types
            for collection-arr = (coerce collection 'vector)
            for table = (case (intern (string-upcase type) :keyword)
                          (:user "users")
                          (:keychain "keychain")
                          (:persona "personas")
                          (:board "boards")
                          (:note "notes")
                          (:file "notes")
                          (:invite "invites")) do
        (unless table
          (error (format nil "bad sync type: ~a" type)))
        (push (link-sync-items collection-arr table) actions))
      ;; once our objects finish linking, flatten our groups and sort by sync id
      ;; ascending
      (chain (all actions)
        (:then (completed)
          (let ((ungrouped nil))
            (dolist (collection completed)
              (loop for record across collection do
                (remhash "rel" record)
                (remhash "item_id" record)
                (push record ungrouped)))
            (let* ((latest-sync-id (if (zerop (length ungrouped))
                                       ""
                                       (gethash "id" (car ungrouped))))
                   (sorted (sort ungrouped (lambda (a b)
                                             (let ((a-sid (hget a '("id")))
                                                   (b-sid (hget b '("id"))))
                                               (when (string< latest-sync-id a-sid)
                                                 (setf latest-sync-id a-sid))
                                               (when (string< latest-sync-id b-sid)
                                                 (setf latest-sync-id b-sid))
                                               (string< a-sid b-sid))))))
              (values sorted latest-sync-id))))))))

(adefun process-incoming-sync (user-id sync)
  "Applies a single sync item against a user's profile."
  (let* ((type (intern (string-upcase (string (gethash "type" sync))) :keyword))
         (action (intern (string-upcase (string (gethash "action" sync))) :keyword))
         (item (gethash "data" sync))
         (item-id (gethash "id" item)))
    (unless (find action '(:add :edit :delete))
      (error (format nil "Bad action given while syncing (~a)" action)))
    (flet ((standard-delete (del-promise)
             (alet* ((sync-ids del-promise))
               (hash ("id" item-id)
                     ("sync_ids" sync-ids)))))
      (vom:debug "bulk sync: ~s ~s ~a" type action item-id)
      (case type
        (:user
          (case action
            (:edit
              (edit-user item-id user-id item))
            ;; only allow edit of user via sync
            (t (error "Only the `edit` action is allowed when syncing user data"))))
        (:keychain
          (case action
            (:add
              (add-keychain-entry user-id item))
            (:edit
              (edit-keychain-entry user-id item-id item))
            (:delete
              (standard-delete (delete-keychain-entry user-id item-id)))))
        (:persona
          (case action
            (:add
              (add-persona user-id item))
            (:edit
              (edit-persona user-id item-id item))
            (:delete
              (standard-delete (delete-persona user-id item-id)))))
        (:board
          (case action
            (:add
              (add-board user-id item))
            (:edit
              (edit-board user-id item-id item))
            (:delete
              (standard-delete (delete-board user-id item-id)))))
        (:note
          (case action
            (:add
              (add-note user-id item))
            (:edit
              (edit-note user-id item-id item))
            (:delete
              (standard-delete (delete-note user-id item-id)))))
        (:file
          (case action
            (:delete
              (standard-delete (delete-note-file user-id item-id)))))
        ;; TODO: invites? yes? no?
        (t
          (error (format nil "Unknown sync record given (~a)" type)))))))

(adefun bulk-sync (user-id sync-items &key request)
  "Given a set of bulk items to sync to a user's profile, run them each. This is
   done sequentially in order to catch errors (and preserve order in the case of
   errors)."
  (let ((successes nil)
        (track-failed sync-items)
        (error nil))
    (chain (aeach (lambda (sync)
                    (alet* ((item (process-incoming-sync user-id sync)))
                      ;; pop the failure that corresponds to this item off the
                      ;; head of the fails list
                      (setf track-failed (cdr track-failed))
                      (let* ((sync-ids (gethash "sync_ids" item))
                             (type (gethash "type" sync))
                             (action (gethash "action" sync)))
                        (remhash "sync_ids" item)
                        (push (hash ("id" (gethash "id" sync))
                                    ("type" type)
                                    ("action" action)
                                    ("sync_ids" sync-ids)
                                    ("data" item))
                              successes))))
                  sync-items)
      (:catch (err) (setf error err))
      (:then ()
        (dolist (sync successes)
          (let* ((type (gethash "type" sync))
                 (action (gethash "action" sync))
                 (track-action (string-downcase (format nil "~a-~a" type action))))
            (track track-action nil request)))
        (hash ("success" (nreverse successes))
              ("fail" (mapcar (lambda (x) (gethash "id" x)) track-failed))
              ("error" error))))))

(adefun delete-sync-items (user-id &key only-affects-user)
  "Delete sync records by user id, with the option of only deleting records that
   affect that one user."
  (alet* ((sock (db-sock))
          (query (r:r (:delete
                        (:filter
                          (:between
                            (:table "sync")
                            (list user-id (:minval))
                            (list user-id (:maxval))
                            :index (db-index "sync" "scan_user"))
                          (r:fn (s)
                            (if only-affects-user
                                (:== (:count (:attr s "rel")) 1)
                                t))))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    t))

(adefun convert-board-share-to-sync (board-id)
  "Given a board's data, return a promise that is resolved with a VECTOR of
   sync items that add the correct board(s) and note(s)."
  (multiple-promise-bind (boards notes)
      (get-board-tree board-id)
    (concatenate
      'vector
      (map 'vector (lambda (board) (convert-to-sync board "board")) boards)
      (map 'vector (lambda (note) (convert-to-sync note "note")) notes))))

(adefun convert-board-unshare-to-sync (user-id board-id)
  "Given a board's data, return a promise that is resolved with a VECTOR of
   sync items that delete the correct board(s) and note(s).
   
   NOTE that we have to be careful of situations where a note can be in two
   shared boards, and if one of the boards is unshared, we do NOT delete the
   note because it is still shared via the other board. Same goes for child
   boards...Board A owns Board B, if Board A is unshared but Board B has a
   separate and valid share to the same persona, Board B must NOT be deleted.

   Also, once we find all the items we do want to delete, we need to sync
   delete the keychain entries as well.
   
   Adding is much easier than deleting =]."
  (multiple-promise-bind (boards notes)
      (get-board-tree
        board-id
        :user-id user-id
        :perm-filter (lambda (type user-id data board-perms)
                       (case type
                         (:board 
                           (let* ((cur-board-id (gethash "id" data))
                                  (perm-entry (gethash cur-board-id board-perms)))
                             ;; remove any boards we still have some level of
                             ;; permissions for. this includes the board being
                             ;; unshared
                             (and perm-entry
                                  (< 0 (gethash "perms" perm-entry 0)))))
                         (:note
                           (user-can-read-note-p user-id data board-perms)))))
    (concatenate
      'vector
      (map 'vector (lambda (board) (convert-to-sync board "board" :action "delete")) boards)
      (map 'vector (lambda (note) (convert-to-sync note "note" :action "delete")) notes))))

(defafun cleanup-sync (future) ()
  "Remove all sync items older than 30 days."
  (alet* ((timestamp (- (get-timestamp) 2592000))
          (sync-id (format nil "~8,'0X0000000000000000" timestamp))
          (sock (db-sock))
          (query (r:r
                   (:delete
                     (:between
                       (:table "sync")
                       (:minval)
                       sync-id))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future t)))


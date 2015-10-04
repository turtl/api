(in-package :turtl)

(route (:get "/sync") (req res)
  "Given the current user and a sync-id, spits out all data that has changes in
   the user's profile since that sync id. Used by various clients to stay in
   sync with the canonical profile (hosted on the server)."
  (alet* ((user-id (user-id req))
          (immediate (not (zerop (varint (get-var req "immediate") 0))))
          (sync-id (get-var req "sync_id")))
    (multiple-promise-bind (sync latest-sync-id)
        (sync-all user-id sync-id :poll (not immediate))
      ;; post-process some of our sync data
      ;; load our boards' persona/invite data
      ;; TODO: since we load the boards independent of the sync system here,
      ;; maybe signal the sync to NOT link against boards? (just return IDs)
      (let ((board-idx (hash))
            (invites nil))
        ;; index a board_id -> board_data hash. we're going to link personas and
        ;; invites and such using this index
        (dolist (rec sync)
          ;; remove server tokens from invites
          (when (string= (gethash "type" rec) "invite")
            (push (gethash "data" rec) invites)
            (remhash "token_server" (gethash "data" rec)))
          (when (string= (gethash "type" rec) "board")
            (let ((board-data (gethash "data" rec)))
              (setf (gethash (gethash "id" board-data) board-idx) board-data))))
        ;; load the boards from the sync
        (alet* ((board-ids (loop for x being the hash-keys of board-idx collect x))
                (nil (populate-invites-personas (coerce invites 'vector)))
                (linked-boards (if (zerop (length board-ids))
                                   #()
                                   (get-boards-by-ids board-ids :get-personas t :get-invites t))))
          ;; for each loaded board w/ personas/privs, set the extra data into
          ;; the indexed board from the sync (desctructive modify)
          (loop for board across linked-boards
                for indexed = (gethash (gethash "id" board) board-idx) do
            (setf (gethash "personas" indexed) (gethash "personas" board)
                  (gethash "privs" indexed) (gethash "privs" board)))
          ;; grab the highest global sync-id. if we have no sync items, we'll
          ;; send this back. this not only keeps the client more up-to-date
          ;; on the sync process, it cuts back on the amount of items we have
          ;; to filter through when syncing since a lot of times we filter on
          ;; the id index.
          (if sync
              (send-json res (hash ("sync_id" latest-sync-id)
                                   ("records" sync)))
              (send-json res nil)))))))

(route (:get "/sync/full") (req res)
  "Called by the client if a user has no local profile data. Returns the profile
   data in the same format as a sync call, allowing the client to process it the
   same way as regular syncing."
  (let ((user-id (user-id req)))
    ;; note we load everything in parallel here to speed up loading
    (alet ((user (get-user-by-id user-id))
           (keychain (get-user-keychain user-id))
           (personas (get-user-personas user-id))
           (global-sync-id (get-latest-sync-id)))
      ;; notes require all our board ids, so load them here
      (alet* ((boards (get-all-boards user-id (map 'list (lambda (p) (gethash "id" p)) personas) :get-invites t))
              (board-ids (map 'list (lambda (b) (gethash "id" b)) boards))
              (notes (get-all-notes user-id board-ids))
              ;; this is a weird case we need to handle. basically, notes in
              ;; the old data model just got 1 board id and would be pulled
              ;; out via that board id. now, notes can have multiple boards so
              ;; are pulled out by board ids AND user id. if we get a note
              ;; by user id that has a board_id we don't have access to, it
              ;; means we added a note to a shared board we no longer are a
              ;; member of, and that note is now undecryptable (to us). in
              ;; this case, we just remove the note.
              ;; 
              ;; NOTE that this only happens in old notes (with a "board_id")
              ;; vs new notes (w/ "boards") because new notes will always have
              ;; an accompanying keychain entry for that note (in case it's
              ;; not in any boards).
              (notes (remove-if (lambda (note)
                                  (let ((board-id (gethash "board_id" note)))
                                    (and board-id
                                         (not (find board-id board-ids :test 'string=)))))
                                notes))
              (files (remove-if-not (lambda (note)
                                      (and (hget note '("file"))
                                           (hget note '("file" "id"))))
                                    notes))
              (files (map 'vector (lambda (x) (copy-hash x)) files))
              (invites (get-persona-invites (map 'list (lambda (p) (gethash "id" p)) personas)))
              (invites (populate-invites-personas invites))
              (sync nil))
        (flet ((convert-to-sync (item type)
                 (let ((rec (make-sync-record (gethash "user_id" item)
                                              type 
                                              (gethash "id" item)
                                              "add")))
                   (setf (gethash "data" rec) item)
                   rec)))
          ;; package it all up
          (push (convert-to-sync user "user") sync)
          (loop for (collection . type) in (list (cons keychain "keychain")
                                                 (cons personas "persona")
                                                 (cons boards "board")
                                                 (cons notes "note")
                                                 (cons files "file")
                                                 (cons invites "invite")) do
            (loop for item across collection do
              (push (convert-to-sync item type) sync))))
        (send-json res (hash ("sync_id" global-sync-id)
                             ("records" (nreverse sync))))))))

(route (:post "/sync") (req res)
  "Bulk sync API. Accepts any number of sync items and applies the updates to
   the profile of the authed user.
   
   Note that the items are added in sequence and if any one in the sequence
   fails, we abort and send back the successes and failures. This is because
   many of the items need to be added in a specific sequence in order to work
   correctly (for instance, a keychain entry for a board needs to be synced
   before the board itself). Catching a failure in the sequence allows the
   client to try again whilst still preserving the original order of the sync
   items."
  (alet* ((user-id (user-id req))
          (sync-items (jonathan:parse (babel:octets-to-string (request-body req)) :as :hash-table))
          ;; i would normally turn my nose up at passing the req object to a
          ;; model, but this is the easiest way to get analytics for bulk
          ;; syncs
          (synced (bulk-sync user-id sync-items :request req)))
    (send-json res synced)))


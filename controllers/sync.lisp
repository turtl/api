(in-package :turtl)

(route (:get "/sync") (req res)
  "Given the current user and a sync-id, spits out all data that has changes in
   the user's profile since that sync id. Used by various clients to stay in
   sync with the canonical profile (hosted on the server).

   Unlike the /sync/full call, this is stateful...we are syncing actual profile
   changes here and thus depend on syncing the correct data. A mistake here can
   put bad data into the profile that will sit there until the app clears its
   local data. So we have to be careful to sync exactly what the client needs.
   This is easy for tangible things like editing a note or adding a keychain
   because there is a 1:1 mapping of sync record -> action. When things get
   tricky is for 'share' and 'unshare' sync records: we have to create a bunch
   of fake sync records that add the board(s) and their note(s) to the profile
   and make sure they are injected at the correct place in the sync result.

   So in the cases where we're fabricating sync items, we have to be cautious
   to add/remove the correct data or the app is going to have a bad time."
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
            (invites nil)
            (shares nil))
        ;; do some sync "tampering" (processing (un)shares, removing sensitive
        ;; data from records (like invite server tokens), index our boards into
        ;; a hash table, and mark any sync records missing their cor data as
        ;; such
        (dolist (rec sync)
          (let ((data (gethash "data" rec)))
            (if data
                (progn 
                  ;; if we have a "share", convert it to a vector of "add" sync
                  ;; items
                  (when (string= (gethash "action" rec) "share")
                    ;; convert the CURRENT sync item to a nop (remove later)
                    (setf (gethash "action" rec) "nop")
                    (case (intern (string-upcase (gethash "type" rec)) :keyword)
                      ;; can only share boards atm
                      (:board
                        (push (convert-board-share-to-sync (gethash "id" data)) shares))))
                  ;; if we have an "unshare", convert it to a vector of "delete"
                  ;; sync items
                  (when (string= (gethash "action" rec) "unshare")
                    ;; convert the CURRENT sync item to a nop (remove later)
                    (setf (gethash "action" rec) "nop")
                    (case (intern (string-upcase (gethash "type" rec)) :keyword)
                      ;; can only unshare boards atm
                      (:board
                        (push (convert-board-unshare-to-sync user-id (gethash "id" data)) shares))))
                  ;; remove server tokens from invites
                  (when (string= (gethash "type" rec) "invite")
                    (push data invites)
                    (remhash "token_server" data))
                  ;; index a board_id -> board_data hash. we're going to link
                  ;; personas and invites and such using this index
                  (when (string= (gethash "type" rec) "board")
                    (let ((board-data data))
                      (setf (gethash (gethash "id" board-data) board-idx) board-data))))
                ;; hmm, looks like the record is missing. let the app know
                (setf (gethash "missing" rec) t))))
        ;; post-process our sync data. if we have shares, wait for them to
        ;; finalize their returned data, then concat the data to the extras
        ;; collection (appended to the end of the sync).
        ;; 
        ;; also, grab personas/privs for each board, and make sure invites have
        ;; personas as well.
        (alet* ((extras (all shares))
                (extras (apply 'concatenate 'vector extras))
                (board-ids (loop for x being the hash-keys of board-idx collect x))
                (nil (populate-invites-personas (coerce invites 'vector)))
                (linked-boards (if (zerop (length board-ids))
                                   #()
                                   (get-boards-by-ids board-ids :get-personas t))))
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
                                   ("records" (remove-if
                                                (lambda (sync)
                                                  (string= "nop" (gethash "action" sync)))
                                                (concatenate 'vector sync extras)))))
              (send-json res nil)))))))

(route (:get "/sync/full") (req res)
  "Called by the client if a user has no local profile data. Returns the profile
   data in the same format as a sync call, allowing the client to process it the
   same way as regular syncing.
   
   It's important to note that this isn't stateful in the sense that we need to
   gather the correct sync items and send them...what we're doing is pulling out
   all the needed data for the profile and returning it as sync 'add' items. Any
   time the app needs a fresh set of *correct* data it can wipe its local data
   and grab this."
  (let ((user-id (user-id req)))
    ;; note we load everything in parallel here to speed up loading
    (alet ((user (get-user-by-id user-id))
           (keychain (get-user-keychain user-id))
           (personas (get-user-personas user-id))
           (global-sync-id (get-latest-sync-id)))
      ;; notes require all our board ids, so load them here
      (alet* ((boards (get-all-boards user-id (map 'list (lambda (p) (gethash "id" p)) personas) :grab-child-boards t))
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
                                      (let ((file (hget note '("file"))))
                                        (and file
                                             (hash-table-p file)
                                             (or (hget file '("id"))
                                                 (hget file '("hash"))))))
                                    notes))
              (files (map 'vector (lambda (x) (copy-hash x)) files))
              (invites (get-persona-invites (map 'list (lambda (p) (gethash "id" p)) personas)))
              (invites (populate-invites-personas invites))
              (sync nil))
        ;; package it all up
        (push (convert-to-sync user "user") sync)
        (loop for (collection . type) in (list (cons keychain "keychain")
                                               (cons personas "persona")
                                               (cons boards "board")
                                               (cons notes "note")
                                               (cons files "file")
                                               (cons invites "invite")) do
          (loop for item across collection do
            (push (convert-to-sync item type) sync)))
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


(in-package :turtl)

(defroute (:get "/api/v2/sync") (req res)
  "Given the current user and a sync-id, spits out all data that has changes in
   the user's profile since that sync id. Used by various clients to stay in
   sync with the canonical profile (hosted on the server)."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (immediate (not (zerop (varint (get-var req "immediate") 0))))
            (sync-id (get-var req "sync_id")))
      (multiple-promise-bind (sync latest-sync-id)
          (sync-all user-id sync-id :poll (not immediate))
        ;; grab the highest global sync-id. if we have no sync items, we'll
        ;; send this back. this not only keeps the client more up-to-date
        ;; on the sync process, it cuts back on the amount of items we have
        ;; to filter through when syncing since a lot of times we filter on
        ;; the id index.
        (if sync
            (send-json res (hash ("sync_id" latest-sync-id)
                                 ("records" sync)))
            (alet* ((global-sync-id (get-latest-sync-id)))
              (send-json res nil)))))))

(defroute (:get "/api/v2/sync/full") (req res)
  "Called by the client if a user has no local profile data. Returns the profile
   data in the same format as a sync call, allowing the client to process it the
   same way as regular syncing."
  (catch-errors (res)
    (let ((user-id (user-id req)))
      ;; note we load everything in parallel here to speed up loading
      (alet ((user (get-user-by-id user-id))
             (keychain (get-user-keychain user-id))
             (personas (get-user-personas user-id))
             (global-sync-id (get-latest-sync-id)))
        ;; notes require all our board ids, so load them here
        (alet* ((boards (get-all-boards user-id (map 'list (lambda (p) (gethash "id" p)) personas)))
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
                ;; vs new news (w/ "boards") because new notes will always have
                ;; an accompanying keychain entry for that note (in case it's
                ;; not in any boards).
                (notes (remove-if (lambda (note)
                                    (let ((board-id (gethash "board_id" note)))
                                      (and board-id
                                           (not (find board-id board-ids :test 'string=)))))
                                  notes))
                (files (remove-if-not (lambda (note)
                                        (and (hget note '("file"))
                                             (hget note '("file" "hash"))))
                                      notes))
                (files (map 'vector (lambda (x) (copy-hash x)) files))
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
                                                   (cons files "file")) do
              (loop for item across collection do
                (push (convert-to-sync item type) sync))))
          (send-json res (hash ("sync_id" global-sync-id)
                               ("records" (nreverse sync)))))))))

(defroute (:post "/api/v2/sync") (req res)
  "Bulk sync API. Accepts any number of sync items and applies the updates to
   the profile of the authed user.
   
   Note that the items are added in sequence and if any one in the sequence
   fails, we abort and send back the successes and failures. This is because
   many of the items need to be added in a specific sequence in order to work
   correctly (for instance, a keychain entry for a board needs to be synced
   before the board itself). Catching a failure in the sequence allows the
   client to try again whilst still preserving the original order of the sync
   items."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-items (jonathan:parse (babel:octets-to-string (request-body req)) :as :hash-table))
            (synced (bulk-sync user-id sync-items)))
      (send-json res synced))))


;;; ----------------------------------------------------------------------------
;;; deprecated stuff
;;; ----------------------------------------------------------------------------

(defroute (:post "/api/sync") (req res)
  "Given the current user and a sync-id, spits out all data that has changes in
   the user's profile since that sync id. Used by various clients to stay in
   sync with the canonical profile (hosted on the server)."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-id (post-var req "sync_id")))
      (alet ((user-sync (sync-user user-id sync-id))
             (keychain-sync (sync-user-keychain user-id sync-id))
             (persona-sync (sync-user-personas user-id sync-id))
             (board-sync (sync-user-boards user-id sync-id :get-personas t))
             (note-sync (sync-user-notes user-id sync-id))
             (file-sync (sync-user-files user-id sync-id))
             ;; grab the highest global sync-id. if we have no sync items, we'll
             ;; send this back. this not only keeps the client more up-to-date
             ;; on the sync process, it cuts back on the amount of items we have
             ;; to filter through when syncing since a lot of times we filter on
             ;; the id index.
             (global-sync-id (get-latest-sync-id)))
        (let ((response (make-hash-table :test #'equal))
              (greatest-sync-id nil))
          (setf (gethash "user" response) user-sync
                (gethash "keychain" response) keychain-sync
                (gethash "personas" response) persona-sync
                (gethash "notes" response) note-sync
                (gethash "files" response) file-sync
                (gethash "boards" response) board-sync)
          ;; out of all the synced items, grab the highest sync id (the client
          ;; can use this as a reference for subseqent calls). this is easy
          ;; since all items in each collection are sync_id sorted Z-A so we can
          ;; just grab the first item from each collection.
          (loop for sync-collection being the hash-values of response do
            (when (< 0 (length sync-collection))
              (let ((sync-id (gethash "id" (gethash "_sync" (aref sync-collection 0)))))
                (when (or (not greatest-sync-id)
                          (string< greatest-sync-id sync-id))
                  (setf greatest-sync-id sync-id)))))
          ;; if we found a sync id in our list of items, return it, otherwise
          ;; use the global sync id.
          (setf (gethash "sync_id" response) (if greatest-sync-id
                                                 greatest-sync-id
                                                 global-sync-id))
          (send-json res response))))))


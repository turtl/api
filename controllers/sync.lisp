(in-package :turtl)

(defroute (:get "/api/v2/sync") (req res)
  "Given the current user and a sync-id, spits out all data that has changes in
   the user's profile since that sync id. Used by various clients to stay in
   sync with the canonical profile (hosted on the server)."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-id (get-var req "sync_id")))
      (multiple-promise-bind (sync latest-sync-id)
          (sync-all user-id sync-id :poll t)
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
             (boards (get-user-boards user-id :get-persona-boards t :get-personas t))
             (global-sync-id (get-latest-sync-id)))
        ;; notes require all our board ids, so load them here
        (alet ((notes (get-notes-from-board-ids (map 'list (lambda (b) (gethash "id" b)) boards)))
               (sync nil))
          (flet ((convert-to-sync (item type)
                   (let ((rec (make-sync-record (gethash "user_id" item)
                                                type 
                                                (gethash "id" item)
                                                "add")))
                     (setf (gethash "_sync" item) rec)
                     item)))
            ;; package it all up
            (push (convert-to-sync user "user") sync)
            (format nil "synccc: ~a~%" global-sync-id)
            (loop for (collection . type) in (list (cons keychain "keychain")
                                                   (cons personas "persona")
                                                   (cons boards "board")
                                                   (cons notes "note")) do
              (loop for item across collection do
                (push (convert-to-sync item type) sync))))
          (send-json res (hash ("sync_id" global-sync-id)
                               ("records" (nreverse sync)))))))))

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


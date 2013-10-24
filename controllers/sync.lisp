(in-package :turtl)

(defroute (:get "/api/sync") (req res)
  (catch-errors (res)
    (send-json res (get-timestamp))))

(defroute (:post "/api/sync") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-id (post-var req "sync_id")))
      (alet ((user-sync (sync-user user-id sync-id))
             (keychain-sync (sync-user-keychain user-id sync-id))
             (persona-sync (sync-user-personas user-id sync-id))
             (board-sync (sync-user-boards user-id sync-id :get-personas t))
             (note-sync (sync-user-notes user-id sync-id))
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

;; NOTE: this route is unused and will remain so until personas have obscurity
;; again
;(defroute (:post "/api/sync/personas/([0-9a-f-]+)") (req res args)
;  (catch-errors (res)
;    (alet* ((persona-id (car args))
;            (challenge (post-var req "challenge")))
;      (with-valid-persona (persona-id challenge)
;        (alet* ((sync-time (varint (post-var req "time") 999999999))
;                (board-sync (sync-persona-boards persona-id sync-time))
;                (note-sync (sync-persona-notes persona-id sync-time))
;                (response (make-hash-table :test #'equal)))
;          (setf (gethash "time" response) (get-timestamp)
;                (gethash "notes" response) note-sync
;                (gethash "boards" response) board-sync)
;          (send-json res response))))))


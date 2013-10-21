(in-package :turtl)

(defroute (:get "/api/sync") (req res)
  (catch-errors (res)
    (send-json res (get-timestamp))))

(defroute (:post "/api/sync") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-time (varint (post-var req "time") 999999999)))
      (alet ((user-sync (sync-user user-id sync-time))
             (keychain-sync (sync-user-keychain user-id sync-time))
             (persona-sync (sync-user-personas user-id sync-time))
             (board-sync (sync-user-boards user-id sync-time :get-persona-boards t :get-personas t))
             (note-sync (sync-user-notes user-id sync-time :get-persona-notes t)))
        (let ((response (make-hash-table :test #'equal)))
          (setf (gethash "time" response) (get-timestamp)
                (gethash "user" response) user-sync
                (gethash "keychain" response) keychain-sync
                (gethash "personas" response) persona-sync
                (gethash "notes" response) note-sync
                (gethash "boards" response) board-sync)
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


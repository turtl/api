(in-package :tagit)

(defroute (:get "/api/sync") (req res)
  (catch-errors (res)
    (send-json res (get-timestamp))))

(defroute (:post "/api/sync") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-time (varint (post-var req "time") 999999999))
            (note-sync (sync-notes user-id sync-time))
            (response (make-hash-table :test #'equal)))
      (setf (gethash "time" response) (get-timestamp)
            (gethash "notes" response) note-sync)
      (send-json res response))))


(in-package :tagit)

(defroute (:get "/api/sync") (req res)
  (catch-errors (res)
    (send-json res (get-timestamp))))

(defroute (:post "/api/sync") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-time (or (parse-integer (post-var req "time") :junk-allowed t) 99999999))
            (note-sync (sync-notes user-id sync-time)))
      (let ((response (make-hash-table :test #'equal)))
        (setf (gethash "time" response) (get-timestamp)
              (gethash "notes" response) note-sync)
        (send-json res response)))))


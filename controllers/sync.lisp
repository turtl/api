(in-package :tagit)

(defroute (:get "/api/sync") (req res)
  (catch-errors (res)
    (send-json res (get-timestamp))))

(defroute (:post "/api/sync") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (sync-time (post-var req "time"))
            (notes (get-user-notes user-id project-id)))
      (sync-changes user-id ))))


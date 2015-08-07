(in-package :turtl)

(defroute (:post "/api/boards/([0-9a-f-]+)/invites") (req res args)
  "Invite a person to a board."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (data (post-body req)))
      )
    )
  )

(defroute (:get "/api/boards/([0-9a-f-]+)/invites/([0-9a-f-]+)") (req res args)
  "Get an invite by board/invite IDs."
  (catch-errors (res)
    )
  )

(defroute (:put "/api/boards/([0-9a-f-]+)/invites/([0-9a-f-]+)/accept") (req res args)
  "Accept an invite by board/invite ID."
  (catch-errors (res)
    )
  )

(defroute (:delete "/api/boards/([0-9a-f-]+)/invites/([0-9a-f-]+)") (req res args)
  "Reject/wemove an invite by board/invite ID."
  (catch-errors (res)
    )
  )

(defroute (:delete "/api/boards/([0-9a-f-]+)/persona/([0-9a-f-]+)") (req res args)
  "Remove a shared persona from a board."
  )

;;; ----------------------------------------------------------------------------
;;; obsolete
;;; ----------------------------------------------------------------------------

(defroute (:put "/api/boards/([0-9a-f-]+)/invites/persona/([0-9a-f-]+)") (req res args)
  "Invite a persona to a board."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (board-id (car args))
            (to-persona-id (cadr args))
            (from-persona-id (post-var req "from_persona"))
            (permissions (varint (post-var req "permissions") nil)))
      (multiple-promise-bind (priv-entry sync-ids)
          (invite-persona-to-board user-id board-id from-persona-id to-persona-id permissions)
        (track "invite" `(:persona t) req)
        (let ((hash (convert-alist-hash priv-entry)))
          (setf (gethash "sync_ids" hash) sync-ids)
          (send-json res hash))))))

(defroute (:put "/api/boards/([0-9a-f-]+)/persona/([0-9a-f-]+)") (req res args)
  "Accept a board invite (persona-intiated)."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (board-id (car args))
            (persona-id (cadr args))
            (sync-ids (accept-board-invite user-id board-id persona-id))
            (board (get-board-by-id board-id :get-notes t)))
      (track "invite-accept" `(:persona t) req)
      (setf (gethash "sync_ids" board) sync-ids)
      (send-json res board))))

(defroute (:delete "/api/boards/([0-9a-f-]+)/persona/([0-9a-f-]+)") (req res args)
  "Leave board share (persona-initiated)."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (board-id (car args))
            (persona-id (cadr args))
            (sync-ids (leave-board-share user-id board-id persona-id)))
      (track "board-leave" nil req)
      (let ((hash (make-hash-table :test #'equal)))
        (setf (gethash "sync_ids" hash) sync-ids)
        (send-json res hash)))))


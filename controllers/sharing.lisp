(in-package :turtl)

(route (:post "/boards/([0-9a-f-]+)/invites") (req res args)
  "Invite a person to a board."
  (alet* ((user-id (user-id req))
          (board-id (car args))
          (invite-data (post-body req))
          (invite (create-board-invite user-id board-id invite-data)))
    (track "invite" (list :persona (gethash "has_persona" invite-data)) req)
    (send-json res invite)))

(route (:get "/boards/([0-9a-f-]+)/invites/([0-9a-f-]+)") (req res args)
  "Get a board invite by ID."
  (alet* ((object-id (car args))
          (invite-id (cadr args))
          (invite (get-invite-by-id/object invite-id object-id))
          (nil (populate-invites-personas (vector invite))))
    (send-json res invite)))

(route (:put "/boards/([0-9a-f-]+)/invites/([0-9a-f-]+)/accept") (req res args)
  "Accept an invite by board ID/invite ID."
  (alet* ((user-id (user-id req))
          (board-id (car args))
          (invite-id (cadr args))
          (token (get-var req "token"))
          (to-persona-id (get-var req "to_persona_id"))
          (board (accept-board-invite user-id invite-id board-id token :to-persona-id to-persona-id)))
    (send-json res board)))

(route (:delete "/boards/([0-9a-f-]+)/invites/([0-9a-f-]+)") (req res args)
  "Weject/wemove an invite by board ID/invite ID."
  (alet* ((user-id (user-id req))
          (board-id (car args))
          (invite-id (cadr args))
          (sync-ids (reject-board-invite user-id invite-id board-id)))
    (send-json res (hash ("id" invite-id)
                         ("sync_ids" sync-ids)))))

(route (:delete "/boards/([0-9a-f-]+)/persona/([0-9a-f-]+)") (req res args)
  "Remove a shared persona from a board."
  (alet* ((user-id (user-id req))
          (board-id (car args))
          (persona-id (cadr args))
          (sync-ids (delete-board-persona-link user-id board-id persona-id))
          (board (get-board-by-id board-id :get-privs t)))
    (setf (gethash "sync_ids" board) sync-ids)
    (send-json res board)))

#|
(route (:post "/invites/boards/([0-9a-f-]+)") (req res args)
  "Invite to board."
  (alet* ((user-id (user-id req))
          (board-id (car args))
          (persona-id (post-var req "persona"))
          (to (post-var req "to"))
          (key (post-var req "key"))
          (board-key (post-var req "board_key"))
          (question (post-var req "question"))
          (used-secret-p (< 0 (varint (post-var req "used_secret") 0)))
          (invite (create-board-invite user-id
                                       board-id
                                       persona-id
                                       to
                                       key
                                       board-key
                                       question
                                       used-secret-p)))
    (track "invite" `(:persona nil :used-secret ,used-secret-p) req)
    (send-json res invite)))

(route (:get "/invites/codes/([0-9a-f-]+)") (req res args)
  "Retrieve information about an invite."
  (alet* ((invite-code (car args))
          (invite-id (or (get-var req "invite_id") ""))
          (invite (get-invite-by-id-code invite-id invite-code :get-from-persona t)))
    (if invite
        (send-json res invite)
        (send-response res :status 404 :body "\"Invite not found.\""))))

(route (:post "/invites/accepted/([0-9a-f-]+)") (req res args)
  "Accept an invite."
  (alet* ((user-id (user-id req))
          (invite-id (car args))
          (invite-code (post-var req "code"))
          (persona-id (post-var req "persona"))
          (success (accept-invite user-id invite-id invite-code persona-id)))
    (track "invite-accept" `(:persona nil) req)
    (send-json res success)))

(route (:post "/invites/denied/([0-9a-f-]+)") (req res args)
  "Deny an invite."
  (alet* ((user-id (user-id req))
          (invite-id (car args))
          (invite-code (post-var req "code"))
          (persona-id (post-var req "persona"))
          (sync-ids (deny-invite user-id invite-id invite-code persona-id)))
    (track "invite-deny" `(:persona nil) req)
    (let ((hash (make-hash-table :test #'equal)))
      (setf (gethash "sync_ids" hash) sync-ids)
      (send-json res hash))))

(route (:delete "/invites/([0-9a-f-]+)") (req res args)
  "Delete an invite (aka deny)."
  (alet* ((invite-id (car args))
          (user-id (user-id req))
          (sync-ids (delete-invite user-id invite-id)))
    (track "invite-delete" `(:persona nil) req)
    (let ((hash (make-hash-table :test #'equal)))
      (setf (gethash "sync_ids" hash) sync-ids)
      (send-json res hash))))
|#


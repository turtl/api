(in-package :tagit)

(defroute (:post "/api/invites/boards/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (board-id (car args))
            (persona-id (post-var req "persona"))
            (challenge (post-var req "challenge"))
            (to (post-var req "to"))
            (key (post-var req "key"))
            (board-key (post-var req "board_key"))
            (used-secret-p (< 0 (varint (post-var req "used_secret") 0)))
            (invite (create-board-invite user-id
                                         board-id
                                         persona-id
                                         challenge
                                         to
                                         key
                                         board-key
                                         used-secret-p)))
      (send-json res invite))))

(defroute (:get "/api/invites/codes/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((invite-code (car args))
            (invite-id (get-var req "invite_id"))
            (invite (get-invite-by-code invite-id invite-code)))
      (send-json res invite))))

(defroute (:delete "/api/invites/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((invite-id (car args))
            (user-id (user-id req))
            (nil (delete-invite user-id invite-id)))
      (send-json res t))))


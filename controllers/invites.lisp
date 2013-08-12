(in-package :tagit)

(defroute (:post "/invites/boards/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (board-id (car args))
            (persona-id (post-var req "persona"))
            (challenge (post-var req "challenge"))
            (to (post-var req "to"))
            (key (post-var req "key"))
            (board-key (post-var req "board_key"))
            (used-secret-p (post-var req "used_secret"))
            (invite (send-invite user-id
                                 board-id
                                 persona-id
                                 challenge
                                 to
                                 key
                                 board-key
                                 used-secret-p)))
      (send-json res invite))))

(defroute (:get "/invites/codes/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((invite-code (car args))
            (invite (get-invite-by-code invite-code


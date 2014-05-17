(in-package :turtl)

(defroute (:post "/api/users") (req res)
  "Signup a new user! Takes only an auth token (ie no plaintext data about the
   user) and optionally encryption user options. Returns the user's brand
   spankin' new ID along with the rest of the posted data."
  (catch-errors (res)
    (alet* ((user-data (post-var req "data"))
            (invited-by (post-var req "invited_by"))
            (invited-by (when (and invited-by
                                   (not (string= invited-by "")))
                          invited-by))
            (user (add-user user-data)))
      (when invited-by
        (alet ((inviting-user-id (get-user-id-from-invite-code invited-by)))
          (when inviting-user-id
            (credit-signup invited-by))))
      (track "user-join" (when invited-by `(:from-invite t)) req)
      (send-json res user))))

(defroute (:get "/api/users/([0-9a-f]+)") (req res args)
  "Get a user by ID."
  (catch-errors (res)
    (let ((user-id (car args))
          (cur-user-id (user-id req)))
      (if (string= user-id cur-user-id)
          (alet* ((user (get-user-by-id user-id))
                  (storage (calculate-user-storage user)))
            (setf (gethash "storage" user) storage)
            (send-json res user))
          (error 'insufficient-privileges :msg "You are accessing a user record that doesn't belong to you.")))))

(defroute (:post "/api/auth") (req res)
  "Used to test auth. Notice we don't actually do anything here. If the auth
   failed, the :pre-route hook will catch it. This just gives a place to play
   ping-pong once a user logs in."
  (catch-errors (res)
    (alet* ((user-id (user-id req)))
      (send-json res user-id))))

(defroute (:put "/api/users/([0-9a-f-]+)") (req res args)
  "Update a user's data. This generally means saving the settings in the user's
   encrypted data."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (mod-user-id (car args))
            (user-data (post-var req "data"))
            (user (edit-user user-id mod-user-id user-data)))
      (send-json res user))))


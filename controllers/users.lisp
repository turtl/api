(in-package :turtl)

(defroute (:post "/api/users") (req res)
  "Signup a new user! Takes only an auth token (ie no plaintext data about the
   user) and optionally encryption user options. Returns the user's brand
   spankin' new ID along with the rest of the posted data."
  (catch-errors (res)
    (alet* ((user-data (post-var req "data"))
            (user (add-user user-data)))
      (track "user-join")
      (send-json res user))))

(defroute (:post "/api/auth") (req res)
  "Used to test auth. Notice we don't actually do anything here. If the auth
   failed, the :pre-route hook will catch it. This just gives a place to play
   ping-pong once a user logs in."
  (catch-errors (res)
    (send-json res (user-id req))))

(defroute (:put "/api/users/([0-9a-f-]+)") (req res args)
  "Update a user's data. This generally means saving the settings in the user's
   encrypted data."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (mod-user-id (car args))
            (user-data (post-var req "data"))
            (user (edit-user user-id mod-user-id user-data)))
      (send-json res user))))


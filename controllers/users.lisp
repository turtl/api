(in-package :turtl)

(defroute (:post "/api/users") (req res)
  "Signup a new user! Takes only an auth token (ie no plaintext data about the
   user) and optionally encrypted user options. Returns the user's brand
   spankin' new ID along with the rest of the posted data."
  (catch-errors (res)
    (alet* ((user-data (post-var req "data"))
            (invited-by (post-var req "invited_by"))
            (invited-by (when (and invited-by (not (string= invited-by "")))
                          invited-by))
            (promo-code (post-var req "promo"))
            (promo-code (when (and promo-code (not (string= promo-code "")))
                          promo-code))
            (promo (when promo-code
                     (get-promo-by-code promo-code))))
      (multiple-promise-bind (user used-promo)
          (add-user user-data :promo promo)
        ;; if invited by another user, accredit the inviter. note that we don't
        ;; wait for the call to finish before returning since
        ;; a. we don't want to delay the user creation process at all and
        ;; b. if something goes wrong with the credit, we really don't need to
        ;;    punish the user joining with some weird error message
        (when invited-by
          (alet ((inviting-user-id (get-user-id-from-invite-code invited-by)))
            (when inviting-user-id
              (credit-signup inviting-user-id))))
        (let ((trackdata nil))
          (when invited-by (setf (getf trackdata :from-invite) t))
          (when used-promo (setf (getf trackdata :promo) promo-code))
          (track "user-join" trackdata req))
        (send-json res user)))))

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

;; TODO: delete? this should be handled by sync system now
(defroute (:put "/api/users/([0-9a-f-]+)") (req res args)
  "Update a user's data. This generally means saving the settings in the user's
   encrypted data."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (mod-user-id (car args))
            (user-data (post-var req "data"))
            (user (edit-user user-id mod-user-id user-data)))
      (send-json res user))))

(defroute (:delete "/api/users/([0-9a-f-]+)") (req res args)
  "Remove a traitorous user and their data from Turtl."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (mod-user-id (car args))
            (success (delete-user user-id mod-user-id)))
      (send-json res success))))


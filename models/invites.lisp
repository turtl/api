(in-package :turtl)

(defvalidator validate-invite
  (("id" :type id :required t)
   ("user_id" :type id :required t)
   ("object_id" :type id :required t)
   ("from" :type id :required t)
   ("to" :type string :required t)
   ("has_persona" :type boolean)
   ("has_passphrase" :type boolean)
   ("token_server" :type string)
   ("body" :type string)))

(adefun get-invite-by-id (invite-id)
  "Get an invite by invite id."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "invites") invite-id)))
          (invite (r:run sock query)))
    (r:disconnect sock)
    invite))

(adefun get-invites-by-object-to (object-id to)
  "Checks for existing invites by object id/to."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:get-all
                     (:table "invites")
                     (list object-id to)
                     :index (db-index "invites" "obj_to"))))
          (cursor (r:run sock query))
          (invites (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    invites))

(adefun get-persona-invites (persona-ids &key return-tokens)
  "Get all invites to or from the given persona ids."
  (flet ((get-invites (persona-ids index)
           (alet* ((sock (db-sock))
                   (query (r:r
                            (:get-all
                              (:table "invites")
                              persona-ids
                              :index (db-index "invites" index))))
                   (cursor (r:run sock query))
                   (invites (r:to-array sock cursor)))
             (r:stop/disconnect sock cursor)
             invites)))
    (alet ((to (get-invites persona-ids "to"))
           (from (get-invites persona-ids "from")))
      (let ((invites (concatenate 'vector to from)))
        (if return-tokens
            invites
            (map 'vector (lambda (i) (remhash "token_server" i) i) invites))))))

(adefun populate-invites-personas (invites)
  "Given a collection of invites, pull out from/to personas for each of them.
   Modifies the invite objects destructively, which for our purposes ends up
   being much more of a feature."
  (when (zerop (length invites))
    (return-from populate-invites-personas #()))
  (let ((persona-ids nil))
    (loop for invite across invites
          for from = (gethash "from" invite)
          for has-persona = (gethash "has_persona" invite)
          for to = (and has-persona (gethash "to" invite)) do
      (when from
        (push from persona-ids))
      (when (and has-persona to)
        (push to persona-ids)))
    (alet* ((personas (get-personas-by-ids persona-ids))
            (persona-idx (hash)))
      ;; index our personas
      (loop for persona across personas do
        (setf (gethash (gethash "id" persona) persona-idx) persona))
      (loop for invite across invites
            for from = (gethash "from" invite)
            for has-persona = (gethash "has_persona" invite)
            for to = (and has-persona (gethash "to" invite)) do
        (let ((from-persona (gethash from persona-idx)))
          (when from-persona
            (setf (gethash "from_persona" invite) from-persona)))
        (let ((to-persona (and has-persona
                               (gethash to persona-idx))))
          (when to-persona
            (setf (gethash "to_persona" invite) to-persona))))
      invites)))

(adefun add-invite (user-id invite-data)
  "Add an invite record (low-level)."
  (setf (gethash "user_id" invite-data) user-id)
  (alet* ((nil (validate-invite (invite-data)))
          (sock (db-sock))
          (query (r:r
                   (:insert
                     (:table "invites")
                     invite-data)))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (alet* ((user-ids (if (gethash "has_persona" invite-data)
                          (alet* ((to (gethash "to" invite-data))
                                  (to-persona (get-persona-by-id to))
                                  (invitee (gethash "user_id" to-persona)))
                            (list user-id invitee))
                          (list user-id)))
            (sync-ids (add-sync-record user-id
                                       "invite"
                                       (gethash "id" invite-data)
                                       "add"
                                       :rel-ids user-ids)))
      (setf (gethash "sync_ids" invite-data) sync-ids)
      invite-data)))

(adefun create-board-invite (user-id board-id invite-data)
  "Create an invite record."
  (alet* ((perms (get-user-board-permissions user-id board-id)))
    (when (< perms 3)
      (error 'insufficient-privileges
             :msg "Sorry, you are editing a board you don't have access to."))
    (setf (gethash "type" invite-data) "board"
          (gethash "object_id" invite-data) board-id)
    (alet* ((invite-to (gethash "to" invite-data))
            (invites (get-invites-by-object-to board-id invite-to))
            (exists (< 0 (length invites))))
      (cond (exists
              (let ((invite (aref invites 0)))
                (setf (gethash "sync_ids" invite) #())
                invite))
            (t
              (alet* ((invite (add-invite user-id invite-data))
                      (nil (send-invite invite)))
                invite))))))

(adefun accept-board-invite (user-id board-id invite-id token)
  "Accept a board invite."
  ;; TODO: verify token
  ;; TODO: convert invite to board_persona_link
  ;; TODO: remove invite (sync record "delete")
  )

(adefun send-invite (invite-data)
  "Sends an invite via email (if settings allow)."
  (alet* ((from (get-persona-by-id (gethash "from" invite-data)))
          (is-persona (gethash "has_persona" invite-data)))
    (if is-persona
        (alet* ((to (get-persona-by-id (gethash "to" invite-data)))
                (settings (gethash "settings" to (hash))))
          (multiple-value-bind (val existsp)
              (gethash "notify_invite" settings)
            (when (or val
                      (not existsp))
              (email-board-persona-invite from to))))
        (email-board-email-invite from invite-data))))


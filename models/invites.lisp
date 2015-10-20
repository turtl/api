(in-package :turtl)

(defvalidator validate-invite
  (("id" :type id :required t)
   ("user_id" :type id :required t)
   ("object_id" :type id :required t)
   ("perms" :type integer :required t)
   ("from" :type id :required t)
   ("to" :type string :required t)
   ("has_persona" :type boolean)
   ("has_passphrase" :type boolean)
   ("title" :type string)
   ("token_server" :type string)
   ("body" :type string)))

(adefun get-invite-by-id (invite-id)
  "Get an invite by invite id."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "invites") invite-id)))
          (invite (r:run sock query)))
    (r:disconnect sock)
    invite))

(adefun get-invite-by-id/object (invite-id object-id &key return-token)
  "Get an invite by invite id, and verify it has the given object id."
  (alet* ((invite (get-invite-by-id invite-id)))
    (if (and invite
               (string= (gethash "object_id" invite) object-id))
        (if return-token
            invite
            (prog1 invite
              (remhash "token_server" invite)))
        (error 'not-found :msg "Invite not found"))))

(adefun get-invites-by-object/to (object-id to)
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
  (when (zerop (length persona-ids))
    (return-from get-persona-invites #()))
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

(adefun delete-invite (user-id invite-id)
  "Remove an invite record."
  (alet* ((invite (get-invite-by-id invite-id))
          (from (gethash "from" invite))
          (to (when (gethash "has_persona" invite)
                (gethash "to" invite)))
          (sock (db-sock))
          (query (r:r (:delete
                        (:get (:table "invites") invite-id))))
          (nil (r:run sock query))
          (persona-ids (remove-if-not 'identity (list from to)))
          (personas (get-personas-by-ids persona-ids))
          (user-ids (map 'list (lambda (p) (gethash "user_id" p)) personas))
          (user-ids (append user-ids (list user-id)))
          (sync-ids (add-sync-record user-id
                                     "invite"
                                     invite-id
                                     "delete"
                                     :rel-ids user-ids)))
    sync-ids))

(adefun delete-persona-invites (user-id persona-id)
  "Delete any invites from/to a persona. No permissions checks, so don't call
   this unless you know user-id owns persona-id (or some other check has been
   performed)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:set-union
                     (:coerce-to (:pluck
                                   (:get-all
                                     (:table "invites")
                                     persona-id
                                     :index (db-index "invites" "to"))
                                   (list "id" "from" "to"))
                      "array")
                     (:coerce-to (:pluck
                                   (:get-all
                                     (:table "invites")
                                     persona-id
                                     :index (db-index "invites" "from"))
                                   (list "id" "from" "to"))
                      "array"))))
          (cursor (r:run sock query))
          (invites (r:to-array sock cursor))
          (nil (r:stop sock cursor))
          (query (r:r (:delete (:get-all (:table "invites") persona-id :index (db-index "invites" "to")))))
          (nil (r:run sock query))
          (query (r:r (:delete (:get-all (:table "invites") persona-id :index (db-index "invites" "from")))))
          (nil (r:run sock query))
          (persona-ids (loop for inv across invites
                             for from = (gethash "from" inv)
                             for to = (gethash "to" inv)
                             append (list from to)))
          (personas (get-personas-by-ids persona-ids))
          (persona-idx (let ((idx (hash)))
                         (loop for persona across personas
                               for id = (gethash "id" persona) do
                           (setf (gethash id idx) persona))
                         idx))
          (sync-records (map 'vector
                             (lambda (invite)
                               (let* ((id (gethash "id" invite))
                                      (from (gethash "from" invite))
                                      (to (gethash "to" invite))
                                      (from-user (gethash "user_id" (gethash from persona-idx (hash))))
                                      (to-user (gethash "user_id" (gethash to persona-idx (hash)))))
                                 (make-sync-record user-id
                                                   "invite"
                                                   id
                                                   "delete"
                                                   :rel-ids (remove-if-not 'identity (list from-user to-user)))))
                             invites))
          (nil (insert-sync-records sync-records)))
    (map 'vector (lambda (s) (gethash "id" s)) sync-records)))

(adefun create-board-invite (user-id board-id invite-data)
  "Create an invite record."
  (alet* ((perms (get-user-board-permissions user-id board-id)))
    (when (< perms 3)
      (error 'insufficient-privileges
             :msg "Sorry, you are editing a board you don't have access to."))
    (setf (gethash "type" invite-data) "board"
          (gethash "object_id" invite-data) board-id)
    (alet* ((invite-to (gethash "to" invite-data))
            (invites (get-invites-by-object/to board-id invite-to))
            (exists (< 0 (length invites))))
      (cond (exists
              (let ((invite (aref invites 0)))
                (setf (gethash "sync_ids" invite) #())
                invite))
            (t
              (alet* ((invite (add-invite user-id invite-data))
                      (nil (send-invite invite)))
                invite))))))

(adefun accept-board-invite (user-id invite-id board-id token &key to-persona-id)
  "Accept a board invite."
  (alet* ((invite (get-invite-by-id/object invite-id board-id :return-token t)))
    (unless invite
      (error 'not-found :msg "Invite not found"))
    (unless (string= (gethash "token_server" invite) token)
      (error 'insufficient-privileges :msg "Invalid invite token"))
    (alet* ((from (gethash "from" invite))
            (to (if (gethash "has_persona" invite)
                    (gethash "to" invite)
                    to-persona-id))
            (perms (gethash "perms" invite))
            (nil (unless to
                   (error 'validation-failed :msg "Please provide a valid to persona id")))
            (persona (or (get-persona-by-id to) (hash)))
            (nil (unless (string= user-id (gethash "user_id" persona))
                   (error 'insufficient-privileges :msg "You are trying to accept an invite that is not to you")))
            (link-data (hash ("id" (make-id))
                             ("board_id" board-id)
                             ("from" from)
                             ("to" to)
                             ("perms" perms)))
            (link (add-board-persona-link user-id link-data))
            (sync-ids (delete-invite user-id invite-id))
            (sync-ids (concatenate 'vector
                                   (gethash "sync_ids" link)
                                   sync-ids))
            (board (get-board-by-id board-id :get-privs t)))
      (setf (gethash "sync_ids" board) sync-ids)
      board)))

(adefun reject-board-invite (user-id invite-id board-id)
  "Reject a board invite. This can be done by either the invitee or the inviter."
  (alet* ((invite (get-invite-by-id/object invite-id board-id))
          (perms (get-user-board-permissions user-id board-id)))
    (unless invite
      (error 'not-found :msg "Invite not found"))
    (flet ((do-delete ()
             (delete-invite user-id invite-id)))
      (if (< 2 perms)
          ;; user owns board, remove the invite willy nilly
          (do-delete)
          ;; user is (allegedly) invited to board, let's verify
          (alet* ((to (when (gethash "has_persona" invite)
                        (gethash "to" invite)))
                  (nil (unless to
                         (error 'insufficient-privileges :msg "You can't delete an invite that doesn't have a persona unless you are the inviter.")))
                  (persona (get-persona-by-id to))
                  (nil (unless (string= user-id (gethash "user_id" persona))
                         (error 'insufficient-privileges :msg "You are trying to reject an invite that is not to you"))))
            (do-delete))))))

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


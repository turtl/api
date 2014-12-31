(in-package :turtl)

(defun make-invite-id (item-id to-email)
  "Create a deterministic id based off the item-id and email."
  (sha256 (concatenate 'string item-id ":" to-email)))

(defafun get-invite-by-id (future) (invite-id &key allow-deleted)
  "gibiLOLOL"
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "invites") invite-id)))
          (invite (r:run sock query)))
    (r:disconnect sock)
    (if (and invite
             (or allow-deleted (not (gethash "deleted" invite))))
        (finish future invite)
        (finish future nil))))

(defafun get-invite-by-id-code (future) (invite-id invite-code &key get-from-persona)
  "Get an invite by ID, and make sure its code matches the code given. This is
   the public way to pull invite info, so that a malicious party would have to
   know both the ID and the code to get any of the invite info, there's no way
   to (practically) guess a code and get the invite."
  (alet* ((invite (get-invite-by-id invite-id)))
    (if (and invite
             (string= invite-code (gethash "code" invite)))
        (if get-from-persona
            (alet* ((persona (get-persona-by-id (gethash "from" invite))))
              (setf (gethash "from" invite) persona)
              (finish future invite))
            (finish future invite))
        (finish future nil))))

(defafun make-invite-code (future) (to-email &optional (salt (crypto-random)))
  "Creates a *unique* (as in, not used by other invites) invite code and returns
   it. Invite codes are meant to be random."
  (alet* ((hash (sha256 (format nil "~a:~a:~a:~a" salt (get-universal-time) (get-internal-real-time) to-email)))
          (code (subseq hash (- (length hash) 7)))
          (sock (db-sock))
          (query (r:r (:count
                        (:get-all (:table "invites")
                                  code
                                  :index (db-index "invites" "code")))))
          (num (r:run sock query)))
    (r:disconnect sock)
    ;; loop until we have a unique code
    (if (< 0 num)
        ;; this code is in use, generate a new one (async recurs)
        (alet ((code (make-invite-code to-email (1+ salt))))
          (finish future code))
        ;; code is not being used
        (finish future code))))

(defafun create-invite (future) (type item-id from-persona-id to-email invite-data expire)
  "Create an invite record which has the ability to attach a set of data to a
   new user's account on join."
  (alet ((invite (make-hash-table :test #'equal))
         (invite-id (make-invite-id item-id to-email))
         (code (make-invite-code to-email)))
    (setf (gethash "id" invite) invite-id
          (gethash "code" invite) code
          (gethash "type" invite) type
          (gethash "from" invite) from-persona-id
          (gethash "to" invite) to-email
          (gethash "item_id" invite) item-id
          (gethash "expire" invite) (+ (get-timestamp) expire)
          (gethash "data" invite) invite-data)
    (finish future invite)))

(defafun insert-invite-record (future) (invite)
  "Inserts an invite hash/object into the db."
  (alet* ((invite-id (gethash "id" invite))
          (sock (db-sock))
          (query (r:r (:delete (:get (:table "invites") invite-id))))
          (nil (r:run sock query))
          (query (r:r (:insert (:table "invites") invite)))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future t)))

(defafun create-board-invite (future) (user-id board-id persona-id to-email key board-key question used-secret-p)
  "Create (and send) a board invite. Also creates a stubbed persona for the
   invitee which is tied to them when they accept the invite."
  ;; make sure the persona/board auth check out
  (with-valid-persona (persona-id user-id future)
    (alet* ((exists-invite-id (make-invite-id board-id to-email))
            (exists-invite (get-invite-by-id exists-invite-id))
            (persona (get-persona-by-id persona-id))
            (invite-code (get-user-invite-code user-id)))
      (if (and exists-invite
               (not (gethash "deleted" exists-invite)))
          ;; this email/board-id invite already exists. just resend it
          (alet ((privs (get-board-privs-entry board-id (gethash "id" exists-invite)))
                 (nil (email-board-invite persona exists-invite key invite-code)))
            (setf (gethash "priv" exists-invite) privs)
            (finish future exists-invite))
          ;; new invite, create/insert/send it
          (alet* ((expire (* 3 86400))
                  (invite-data (let ((hash (make-hash-table :test #'equal)))
                                 (setf (gethash "board_key" hash) board-key
                                       (gethash "question" hash) question
                                       (gethash "used_secret" hash) used-secret-p)
                                 hash))
                  (invite (create-invite "b" board-id persona-id to-email invite-data expire))
                  (invite-id (gethash "id" invite)))
            (multiple-promise-bind (nil priv-entry sync-ids)
                (add-board-remote-invite user-id board-id persona-id invite-id 2 to-email)
              (alet* ((nil (insert-invite-record invite))
                      (nil (email-board-invite persona invite key invite-code)))
                (setf (gethash "priv" invite) (convert-alist-hash priv-entry)
                      (gethash "sync_ids" invite) sync-ids)
                (finish future invite))))))))

(defafun invite-persona-to-board (future) (user-id board-id from-persona-id to-persona-id permissions)
  "Invites a persona to join a board, setting all applicable permissions."
  (multiple-promise-bind (nil priv-entry sync-ids)
      (set-board-persona-permissions user-id board-id from-persona-id to-persona-id permissions :invite t)
    (alet* ((from-persona (get-persona-by-id from-persona-id :without-keys t))
            (to-persona (get-persona-by-id to-persona-id :without-keys t))
            (setting-invite (get-persona-setting nil "notify_invite" :persona to-persona :default 1))
            (setting-disable (get-persona-setting nil "disable_all" :persona to-persona :default 0))
            (email-notify (ignore-errors (= setting-invite 1)))
            (disable-all (ignore-errors (= setting-disable 1))))
      ;; don't really need to wait for email to go through here
      (when (and (< 0 permissions)
                 email-notify
                 (not disable-all))
        (alet ((sent (email-board-persona-invite from-persona to-persona)))
          (declare (ignore sent))
          nil)))
    (finish future priv-entry sync-ids)))

(defafun delete-invite-record (future) (invite-id)
  "Delete an invite's record."
  (alet* ((sock (db-sock))
          (query (r:r (:delete (:get (:table "invites") invite-id))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future t)))

(defafun delete-invite (future) (user-id invite-id)
  "Delete an invite."
  (alet* ((invite (get-invite-by-id invite-id :allow-deleted t)))
    (if invite
        (alet* ((invite-type (gethash "type" invite))
                (invite-type-keyword (intern (string-upcase invite-type) :keyword))
                (res (case invite-type-keyword
                       (:b (delete-board-invite user-id invite))))
                (nil (delete-invite-record invite-id)))
          (finish future res))
        (signal-error future (make-instance 'not-found :msg "That invite wasn't found.")))))

(defafun delete-board-invite (future) (user-id invite)
  "Delete a board invite."
  (alet* ((invite-id (gethash "id" invite))
          (board-id (gethash "item_id" invite)))
    (multiple-promise-bind (nil nil sync-ids)
        (set-board-persona-permissions user-id board-id nil invite-id 0)
      (finish future sync-ids))))

(defafun accept-invite (future) (user-id invite-id invite-code persona-id)
  "Accept an invite. Removes the invite record, and updates the object
   (board/note/etc) according to the given persona."
  (alet* ((invite (get-invite-by-id-code invite-id invite-code)))
    (if invite
        (alet* ((invite-id (gethash "id" invite))
                (invite-type (gethash "type" invite))
                (item-id (gethash "item_id" invite))
                (invite-type-keyword (intern (string-upcase invite-type) :keyword))
                (res (let ((subfuture (make-promise)))
                       (case invite-type-keyword
                         (:b (alet* ((sync-ids (accept-board-invite user-id item-id persona-id :invite-id invite-id))
                                     (board (get-board-by-id item-id :get-notes t)))
                               (setf (gethash "sync_ids" board) sync-ids)
                               (finish subfuture board))))))
                (nil (delete-invite-record invite-id)))
          (finish future res))
        (signal-error future (make-instance 'not-found :msg "That invite wasn't found.")))))

(defafun deny-invite (future) (user-id invite-id invite-code persona-id)
  "Deny an invite. Removes the invite record and also marks the invite entry on
   the object attached to the invite as deleted."
  (alet* ((invite (get-invite-by-id-code invite-id invite-code)))
    (if invite
        (alet* ((invite-type (gethash "type" invite))
                (item-id (gethash "item_id" invite))
                (invite-type-keyword (intern (string-upcase invite-type) :keyword))
                (res (case invite-type-keyword
                       (:b (leave-board-share user-id item-id persona-id :invite-id invite-id))))
                (nil (delete-invite-record invite-id)))
          (finish future res))
        (signal-error future (make-instance 'not-found :msg "That invite wasn't found.")))))

(defafun cleanup-invites (future) ()
  "Delete expired invites."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:delete
                     (:filter
                       (:table "invites")
                       (r:fn (c)
                         (:< (:attr c "expire") (- (get-timestamp) 60)))))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future t)))


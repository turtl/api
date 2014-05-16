(in-package :turtl)

(define-condition user-exists (turtl-error)
  ((code :initform 403)))

(define-condition user-mismatch (turtl-error)
  ((code :initform 403)))

(defvalidator validate-user
  (("id" :type string :required t :length 24)
   ("a" :type string :required t)
   ("invite_code" :type string :required t)
   ("body" :type cl-async-util:bytes-or-string)))

(defafun check-auth (future) (auth-key)
  "Check if the given auth key exists. Finishes with the user id if so, nil
   otherwise."
  (alet* ((auth-key (decode-key auth-key))
          (sock (db-sock))
          (query (r:r (:limit
                        (:get-all
                          (:table "users")
                          auth-key
                          :index (db-index "users" "a"))
                        1)))
          (cursor (r:run sock query))
          (res (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (if (and res (< 0 (length res)))
        (let ((user (aref res 0)))
          (cond ((gethash "invite_code" user)
                 ;; user has an invite code. carry on.
                 (finish future user))
                (t
                 ;; no invite code!!!!!!!11 retroactively generate one. this is
                 ;; fine since an upgrade requires logging out anyway.
                 (alet* ((sock (db-sock))
                         (user-id (gethash "id" user))
                         (invite-code (generate-unique-invite-code user-id))
                         (query (r:r (:update
                                       (:get (:table "users") user-id)
                                       `(("invite_code" . ,invite-code)))))
                         (nil (r:run sock query)))
                   (r:disconnect sock)
                   (setf (gethash "invite_code" user) invite-code)
                   (finish future user)))))
        (finish future nil))))

(defafun generate-unique-invite-code (future) (user-id)
  "Given a user ID, generate a unique invite code they can send to others for
   tracking their invites.

   This works by sha256 hashing the user id, taking the first 5 characters of
   the hash, and seeing if an existing code exists. Each time a code exists,
   the code is rolled one character over the hash (so instead of chars 0-5 you
   get chars 1-6). If all 5-character codes in the hash are taken, the hash is
   hashed against itself and the process starts over."
  (alet* ((sock (db-sock))
          (size 5)
          (hash (sha256 user-id))
          (idx 0)
          (next (+ idx size)))
    (labels ((next ()
               (when (<= (length hash) next)
                 (setf hash (sha256 hash)
                       idx 0
                       next (+ idx size)))
               (alet* ((code (subseq hash idx next))
                       (query (r:r (:count (:get-all (:table "users")
                                                    code
                                                    :index (db-index "users" "invite_code")))))
                       (count (r:run sock query)))
                 (cond ((zerop count)
                        (r:disconnect sock)
                        (finish future code))
                       (t
                        (incf idx)
                        (incf next)
                        (next))))))
      (next))))

(defafun get-user-by-id (future) (user-id)
  "Grab a user by ID."
  (alet* ((sock (db-sock))
          (query (r:r (:without (:get (:table "users") user-id) "a")))
          (user (r:run sock query)))
    (r:disconnect sock)
    (finish future user)))

(defafun add-user (future) (user-data)
  "Add a new user"
  (add-id user-data)
  (alet ((user (check-auth (gethash "a" user-data))))
    (if user
        (signal-error future (make-instance 'user-exists
                                            :msg "You are joining with existing login credentials. Did you mean to log in?"))
        (alet* ((user-id (gethash "id" user-data))
                (invite-code (generate-unique-invite-code user-id)))
          (setf (gethash "invite_code" user-data) invite-code)
          (validate-user (user-data future)
            (alet* ((sock (db-sock))
                    (query (r:r (:insert
                                  (:table "users")
                                  user-data)))
                    (nil (r:run sock query))
                    (sync-ids (add-sync-record user-id "user" user-id "add")))
              (r:disconnect sock)
              (setf (gethash "sync_ids" user-data) sync-ids)
              (finish future user-data)))))))

(defafun edit-user (future) (user-id mod-user-id user-data)
  "Edit a user. Mainly used to update a user's private (encrypted) data and
   settings."
   (if (string= user-id mod-user-id)
       (validate-user (user-data future :edit t)
         (alet* ((sock (db-sock))
                 (query (r:r (:update
                               (:get (:table "users") user-id)
                               user-data)))
                 (nil (r:run sock query))
                 (sync-ids (add-sync-record user-id "user" user-id "edit")))
           (r:disconnect sock)
           (setf (gethash "sync_ids" user-data) sync-ids)
           (finish future user-data)))
       (signal-error future (make-instance 'user-mismatch
                                           :msg "You tried to edit someone else's account. For shame."))))

(defafun get-user-data (future) (user-id)
  "Get the private data section (`body` key) for a user."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:get (:table "users") user-id)
                        "body")))
          (user (r:run sock query)))
    (r:disconnect sock)
    (finish future user)))


(in-package :turtl)

(define-condition user-exists (turtl-error)
  ((code :initform 403)))

(define-condition user-mismatch (turtl-error)
  ((code :initform 403)))

(defvalidator validate-user
  (("id" :type string :required t :length 24)
   ("a" :type string :required t)
   ("storage" :type integer :required nil)
   ("invite_code" :type string :required t)
   ("body" :type cl-async-util:bytes-or-string)))

(defafun check-auth (future) (auth-key)
  "Check if the given auth key exists. Finishes with the user id if so, nil
   otherwise."
  (catcher
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
          (finish future nil)))
    (error (e)
      (vom:error "check-auth: ~a" e))))

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

(defafun get-user-invite-code (future) (user-id)
  "Grab a user's invite code by their ID."
  (alet* ((sock (db-sock))
          (query (r:r (:default
                        (:attr
                          (:get (:table "users") user-id)
                          "invite_code")
                        nil)))
          (invite-code (r:run sock query)))
    (r:disconnect sock)
    (finish future invite-code)))

(defafun add-user (future) (user-data &key promo)
  "Add a new user. Optionally pass a promo object."
  (add-id user-data)
  ;; apply the promo to user-object in-place destructively
  (if (and promo
           (apply-promo user-data promo :count-uses t))
      ;; yes we used a promo
      (setf promo t)
      ;; no promo was used
      (setf promo nil))
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
              (finish future user-data promo)))))))

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

(defafun get-user-id-from-invite-code (future) (invite-code)
  "Grab a user's ID from the invite code."
  (alet* ((sock (db-sock))
          (query (r:r (:limit
                        (:get-all
                          (:table "users")
                          invite-code
                          :index (db-index "users" "invite_code"))
                        1)))
          (cursor (r:run sock query))
          (users (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (if (< 0 (length users))
        (finish future (gethash "id" (aref users 0)))
        (finish future nil))))

(defafun credit-signup (future) (user-id)
  "Give a user credit for referring someone."
  (alet* ((sock (db-sock))
          (iname (format nil "~a" *storage-invite-credit*))
          (query (r:r (:update
                        (:get (:table "users") user-id)
                        (r:fn (u)
                          `(("invites" . ((,iname . ,(:+ (:default (:attr (:attr u "invites") iname) 0)
                                                         1)))))))))
          (nil (r:run sock query))
          (sync-ids (add-sync-record user-id
                                     "user"
                                     user-id
                                     "edit")))
    (r:disconnect sock)
    (finish future sync-ids)))

(defun calculate-user-storage (user &key (unit :bytes))
  "Calculate a user's allowed profile size (in bytes). This takes into account
   the number of successful invites the user has sent, but can also be
   overridden by manually setting the `storage` field."
  (let* ((storage (gethash "storage" user))
         (invites (gethash "invites" user))
         (amount (or storage 100))
         (mul (case unit
                (:bytes (* 1024 1024))
                (:megabytes 1)
                (:gigabytes (/ 1 1024)))))
    (when invites
      (loop for k being the hash-keys of invites
            for v being the hash-values of invites do
        (let ((size-mb (ignore-errors (parse-integer k :junk-allowed t))))
          (when size-mb
            (incf amount (* size-mb (round v)))))))
    (round (* mul amount))))


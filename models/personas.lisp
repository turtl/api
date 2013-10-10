(in-package :turtl)

(define-condition persona-email-exists (turtl-error)
  ((code :initform 403)))
  
(defvalidator validate-persona
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("pubkey" :type string :required t)
   ("email" :type string :required t :transform string-downcase)
   ;("screenname" :type string :required t :max-length 24)
   ("name" :type string)
   ("body" :type cl-async-util:bytes-or-string :required t)
   ("settings" :type hash-table)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defafun get-persona-by-id (future) (persona-id &key without-keys)
  "Get a persona by id."
  (alet* ((sock (db-sock))
          (query (r:r (if without-keys
                          (:without
                            (:get (:table "personas") persona-id)
                            "body"
                            "pubkey")
                          (:get (:table "personas") persona-id))))
          (persona (r:run sock query)))
    (r:disconnect sock)
    (finish future persona)))

(defafun get-user-personas (future) (user-id)
  "Get personas by user id."
  (alet* ((sock (db-sock))
          (query (r:r (:filter
                        (:get-all
                          (:table "personas")
                          user-id
                          :index "user_id")
                        (r:fn (persona)
                          (:~ (:default (:attr persona "deleted") nil))))))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future personas)))

(defafun user-personas-map (future) (user-id map-fn &key flatten)
  "Run a function on all of a user's persona's and collect the results as an
   array. The callback takes one argument, the persona id."
  (alet* ((personas (get-user-personas user-id))
          (items nil))
    (if (< 0 (length personas))
        (loop for i = 0
              for persona across personas
              for persona-id = (gethash "id" persona) do
          (alet ((item (funcall map-fn persona-id)))
            (if (and flatten
                     (or (typep item 'list)
                         (typep item 'array)))
                (cl-rethinkdb-util:do-list/vector (subitem item)
                  (push subitem items))
                (push item items))
            (incf i)
            (when (<= i (length personas))
              (finish future (coerce (nreverse items) 'simple-vector)))))
        (finish future #()))))
  
;; TODO: limit number of personas a user can have
(defafun add-persona (future) (user-id persona-data)
  "Add a persona to the system."
  (add-id persona-data)
  (add-mod persona-data)
  (setf (gethash "user_id" persona-data) user-id)
  (validate-persona (persona-data future)
    (when (string= (gethash "pubkey" persona-data) "false")
      (setf (gethash "pubkey" persona-data) nil))
    (aif (persona-email-available-p (gethash "email" persona-data))
         (alet* ((sock (db-sock))
                 (query (r:r (:insert
                               (:table "personas")
                               persona-data)))
                 (nil (r:run sock query)))
           (r:disconnect sock)
           (finish future persona-data))
         (signal-error future (make-instance 'persona-email-exists
                                             :msg "That email is already registered to another persona.")))))

(defafun edit-persona (future) (user-id persona-id persona-data)
  "Update a persona."
  (with-valid-persona (persona-id user-id future)
    (validate-persona (persona-data future :edit t)
      (add-mod persona-data)
      (when (string= (gethash "pubkey" persona-data) "false")
        (setf (gethash "pubkey" persona-data) nil))
      (setf (gethash "user_id" persona-data) user-id)
      ;; make sure settings use numeric value
      (let ((settings (gethash "settings" persona-data))
            (new-settings (make-hash-table :test #'equal)))
        (when (hash-table-p settings)
          (loop for k being the hash-keys of settings
                for v being the hash-values of settings do
            (let ((new-val (ignore-errors (parse-float v))))
              (when new-val
                (setf (gethash k new-settings) new-val))))
          (setf (gethash "settings" persona-data) new-settings)))
      (alet* ((email (gethash "email" persona-data))
              (availablep (or (not email)
                              (persona-email-available-p email persona-id))))
        (if availablep
            (alet* ((sock (db-sock))
                    (query (r:r (:update
                                  (:get (:table "personas") persona-id)
                                  persona-data)))
                    (nil (r:run sock query)))
              (r:disconnect sock)
              (finish future persona-data))
            (signal-error future (make-instance 'persona-email-exists
                                                :msg "That email is taken by another persona.")))))))

(defafun delete-persona (future) (user-id persona-id &key permanent)
  "Delete a persona."
  (with-valid-persona (persona-id user-id future)
    (alet* ((sock (db-sock))
            (query (r:r (if permanent
                            (:delete (:get (:table "personas") persona-id))
                            (:update
                              (:get (:table "personas") persona-id)
                              `(("deleted" . t)
                                ("email" . "")
                                ("pubkey" . "")
                                ("body" . "")
                                ("mod" . ,(get-timestamp)))))))
            (nil (r:run sock query))
            (nil (delete-persona-links persona-id :permanent permanent)))
      (r:disconnect sock)
      (finish future t))))

(defafun delete-persona-links (future) (persona-id &key permanent)
  "Delete all persona-related information. This generally means board-persona
   links."
  (alet* ((sock (db-sock))
          (query-to (r:r (if permanent
                             (:delete (:get-all (:table "boards_personas_link") persona-id :index "to"))
                             (:update
                               (:get-all (:table "boards_personas_link") persona-id :index "to")
                               `(("deleted" . ,t)
                                 ("mod" . ,(get-timestamp)))))))
          (query-from (r:r (if permanent
                               (:delete (:get-all (:table "boards_personas_link") persona-id :index "from"))
                               (:update
                                 (:get-all (:table "boards_personas_link") persona-id :index "from")
                                 `(("deleted" . ,t)
                                   ("mod" . ,(get-timestamp)))))))
          (query-mod (r:r
                       (:foreach
                         (:set-union
                           (:attr (:get-all (:table "boards_personas_link") persona-id :index "to") "board_id")
                           (:attr (:get-all (:table "boards_personas_link") persona-id :index "from") "board_id"))
                         (r:fn (board-id)
                           (:update
                             (:get (:table "boards") board-id)
                             `(("mod" . ,(get-timestamp))))))))
          (nil (r:run sock query-to))
          (nil (r:run sock query-from))
          (nil (r:run sock query-mod)))
    (r:disconnect sock)
    (finish future t)))

(defafun get-persona-by-email (future) (email &key ignore-persona require-key)
  "Grab a persona via its email. Must be an exact match (for now)."
  (alet* ((sock (db-sock))
          (email (string-downcase email))
          (query (r:r (:limit
                        (:get-all (:table "personas")
                                  email
                                  :index "email")
                        1)))
          (cursor (r:run sock query))
          (persona (when (r:has-next cursor)
                     (r:next sock cursor))))
    (r:stop/disconnect sock cursor)
    (if (and (hash-table-p persona)
             (not (string= ignore-persona (gethash "id" persona)))
             (or (not require-key)
                 (gethash "pubkey" persona)))
        (progn
          (remhash "secret" persona)
          (finish future persona))
        (finish future nil))))

(defafun get-persona-setting (future) (persona-id setting-name &key persona default)
  "Get a setting for a persona."
  ;; don't bother pulling out a persona if one was provided
  (alet* ((persona (if persona
                       persona
                       (get-persona-by-id persona-id :without-keys t))))
    (if persona
        (let* ((settings (gethash "settings" persona)))
          (if (hash-table-p settings)
              (multiple-value-bind (val existsp)
                  (gethash setting-name settings)
                (if existsp
                    (finish future val)
                    (finish future default)))
              (finish future default)))
        (finish future default))))

(defafun get-board-personas (future) (board-id)
  "Given a board ID, find all personas that board is shared with and pull them
   out."
  (alet* ((sock (db-sock))
          (query (r:r (:map
                        (:filter
                          (:get-all
                            (:table "boards_personas_link")
                            board-id
                            :index "board_id")
                          (r:fn (link)
                            (:&& (:~ (:== (:attr link "perms") 0))
                                 (:~ (:default (:attr link "deleted") nil)))))
                        (r:fn (link)
                          (:get (:table "personas") (:attr link "to"))))))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future (remove nil personas))))

(defafun persona-email-available-p (future) (email &optional ignore-id)
  "Test whether or not a email is available."
  (aif (get-persona-by-email email :ignore-persona ignore-id)
       (finish future nil)
       (finish future t)))

(defafun persona-owned-by-user-p (future) (persona-id user-id)
  "Determines if a user is the owner of a persona."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:default (:get (:table "personas") persona-id) (make-hash-table))
                        "user_id")))
          (persona (r:run sock query))
          (validp (string= user-id (gethash "user_id" persona))))
    (r:disconnect sock)
    (finish future validp)))

;; NOTE: this function is not used and won't be until personas offer account
;; obscuration again.
(defafun persona-challenge-response-valid-p (future) (persona-id response)
  "Determine if the given response if valid for the persona specified by
   persona-id."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:default (:get (:table "personas") persona-id) (make-hash-table))
                        "secret")))
          (persona (r:run sock query))
          (validp (verify-challenge :persona persona-id (gethash "secret" persona) response)))
    (r:disconnect sock)
    (finish future validp)))


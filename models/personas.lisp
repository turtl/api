(in-package :turtl)

(define-condition persona-email-exists (turtl-error)
  ((code :initform 403)))
  
(defvalidator validate-persona
  (("id" :type id :required t)
   ("user_id" :type id :required t)
   ("pubkey" :type string :required nil)
   ("email" :type string :required t :transform string-downcase)
   ("name" :type string)
   ("body" :type cl-async-util:bytes-or-string :required t)
   ("settings" :type hash-table))
  :old t)

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

(adefun get-personas-by-ids (persona-ids)
  "Get a buttload of personas by ids."
  (when (zerop (length persona-ids))
    (return-from get-personas-by-ids #()))
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "personas")
                        persona-ids)))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    personas))

(defafun get-user-personas (future) (user-id)
  "Get personas by user id."
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "personas")
                        user-id
                        :index (db-index "personas" "user_id"))))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future personas)))

(defafun get-user-persona-ids (future) (user-id)
  "Get a user's persona IDs."
  (alet* ((sock (db-sock))
          (query (r:r (:attr
                        (:get-all
                          (:table "personas")
                          user-id
                          :index (db-index "personas" "user_id"))
                        "id")))
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
  
(defafun add-persona (future) (user-id persona-data)
  "Add a persona to the system."
  (setf (gethash "user_id" persona-data) user-id)
  (let ((cid (gethash "cid" persona-data)))
    (validate-persona (persona-data future)
      (when (string= (gethash "pubkey" persona-data) "false")
        (setf (gethash "pubkey" persona-data) nil))
      (aif (persona-email-available-p (gethash "email" persona-data))
           (alet* ((sock (db-sock))
                   (query (r:r (:insert
                                 (:table "personas")
                                 persona-data)))
                   (nil (r:run sock query))
                   (sync-ids (add-sync-record user-id
                                              "persona"
                                              (gethash "id" persona-data)
                                              "add"
                                              :client-id cid)))
             (r:disconnect sock)
             (setf (gethash "sync_ids" persona-data) sync-ids)
             (finish future persona-data))
           (signal-error future (make-instance 'persona-email-exists
                                               :msg "That email is already registered to another persona."))))))

(defafun edit-persona (future) (user-id persona-id persona-data)
  "Update a persona."
  (with-valid-persona (persona-id user-id future)
    (validate-persona (persona-data future :edit t)
      (when (string= (gethash "pubkey" persona-data) "false")
        (setf (gethash "pubkey" persona-data) nil))
      (setf (gethash "user_id" persona-data) user-id)
      ;; make sure settings translate `false` properly
      (let ((settings (gethash "settings" persona-data)))
        (when (hash-table-p settings)
          (loop for k being the hash-keys of settings
                for v being the hash-values of settings do
            (when (null v)
              (setf (gethash k settings) :false)))))
      (alet* ((email (gethash "email" persona-data))
              (availablep (or (not email)
                              (persona-email-available-p email persona-id))))
        (if availablep
            (alet* ((sock (db-sock))
                    (query (r:r (:replace
                                  (:get (:table "personas") persona-id)
                                  persona-data)))
                    (nil (r:run sock query))
                    (sync-ids (add-sync-record user-id "persona" persona-id "edit")))
              (r:disconnect sock)
              (setf (gethash "sync_ids" persona-data) sync-ids)
              (finish future persona-data))
            (signal-error future (make-instance 'persona-email-exists
                                                :msg "That email is taken by another persona.")))))))

(defafun delete-persona (future) (user-id persona-id)
  "Delete a persona."
  (with-valid-persona (persona-id user-id future)
    (alet* ((nil (delete-persona-links user-id persona-id))
            (nil (delete-persona-invites user-id persona-id))
            (sock (db-sock))
            (query (r:r (:delete (:get (:table "personas") persona-id))))
            (nil (r:run sock query))
            (sync-ids (add-sync-record user-id "persona" persona-id "delete")))
      (r:disconnect sock)
      (finish future sync-ids))))

;; TODO: either move this board model, or move delete-persona-invites to this
;; model.
(adefun delete-persona-links (user-id persona-id)
  "Delete all persona-related information. This generally means board-persona
   links."
  (alet* ((sock (db-sock))
          (query-boards (r:r (:set-union
                               (:coerce-to (:pluck (:get-all (:table "boards_personas_link") persona-id :index (db-index "boards_personas_link" "to")) (list "board_id" "to")) "array")
                               (:coerce-to (:pluck (:get-all (:table "boards_personas_link") persona-id :index (db-index "boards_personas_link" "from")) (list "board_id" "to")) "array"))))
          (cursor (r:run sock query-boards))
          (board-links (if cursor
                           (r:to-array sock cursor)
                           #()))
          (nil (r:stop sock cursor))
          (query-to (r:r (:delete (:get-all (:table "boards_personas_link") persona-id :index (db-index "boards_personas_link" "to")))))
          (query-from (r:r (:delete (:get-all (:table "boards_personas_link") persona-id :index (db-index "boards_personas_link" "from")))))
          (nil (r:run sock query-to))
          (nil (r:run sock query-from))
          (sync-records nil)
          ;; loop over the board links, grab affected users, and push our sync
          ;; records
          (nil (all (loop for link across board-links
                          for board-id = (gethash "board_id" link)
                          for to = (gethash "to" link) collect
                      (alet* ((user-ids (get-affected-users-from-board-ids (list board-id)))
                              (to-persona (get-persona-by-id to))
                              (to-user-id (gethash "user_id" to-persona)))
                        ;; edit the board
                        (push (make-sync-record user-id
                                                "board"
                                                board-id
                                                "edit"
                                                :rel-ids user-ids) sync-records)
                        (push (make-sync-record user-id
                                                "board"
                                                board-id
                                                "unshare"
                                                :rel-ids (list to-user-id)
                                                :no-auto-add-user t) sync-records)))))
          (nil (insert-sync-records sync-records)))
    (r:disconnect sock)
    (map 'vector (lambda (sync) (gethash "id" sync)) sync-records)))

(defafun get-persona-by-email (future) (email &key ignore-persona require-key)
  "Grab a persona via its email. Must be an exact match (for now)."
  (alet* ((sock (db-sock))
          (email (string-downcase email))
          (query (r:r (:limit
                        (:get-all (:table "personas")
                                  email
                                  :index (db-index "personas" "email"))
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
                            :index (db-index "boards_personas_link" "board_id"))
                          (r:fn (link)
                            (:~ (:== (:attr link "perms") 0))))
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


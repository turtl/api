(in-package :turtl)

(defvalidator validate-board
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("keys" :type sequence :required t :coerce simple-vector)
   ("body" :type cl-async-util:bytes-or-string)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defafun populate-boards-data (future) (boards &key get-notes get-personas set-shared)
  "Populate certain information given a list of boards."
  (if (and (< 0 (length boards))
           (or get-notes get-personas set-shared))
      (loop for i = 0
            for board across boards
            for board-id = (gethash "id" board) do
        (alet ((board board) ;; bind for inner form or loop will shit all over it
               (personas (when get-personas (get-board-personas board-id)))
               (notes (when get-notes (get-board-notes board-id))))
          ;; filter out privs = 0 entries
          (when (hash-table-p (gethash "privs" board))
            (loop for persona-id being the hash-keys of (gethash "privs" board)
                  for entry being the hash-values of (gethash "privs" board) do
              (when (and (hash-table-p entry)
                         (or (zerop (gethash "p" entry))
                             (gethash "d" entry)))
                (remhash persona-id (gethash "privs" board)))))
          (when set-shared (setf (gethash "shared" board) t))
          (when (and get-notes notes) (setf (gethash "notes" board) notes))
          (when (and get-personas personas) (setf (gethash "personas" board) personas))
          (incf i)
          (when (<= (length boards) i)
            (finish future boards))))
      (finish future boards)))

(defafun get-user-boards (future) (user-id &key get-persona-boards get-notes get-personas)
  "Get all boards for a user."
  (alet* ((sock (db-sock))
          ;; TODO: implement (:without ... "user_id") once >= RDB 1.8
          (query (r:r (:filter
                        (:get-all
                          (:table "boards")
                          user-id
                          :index "user_id")
                        (r:fn (board)
                          (:~ (:default (:attr board "deleted") nil))))))
          (cursor (r:run sock query))
          (boards (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    ;; grab persona boards and populate all boards with note/personas/etc
    (alet* ((persona-boards (if get-persona-boards
                                (user-personas-map user-id 'get-persona-boards :flatten t)
                                #()))
            (all-boards (cl-async-util:append-array boards persona-boards))
            (boards-populated (populate-boards-data all-boards
                                                    :get-notes get-notes
                                                    :get-personas get-personas)))
      (finish future boards-populated))))

(defafun get-persona-boards (future) (persona-id &key get-notes)
  "Get all boards for a persona."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:filter
                     (:table "boards")
                     (r:fn (board)
                       (:&& (:~ (:default (:attr board "deleted") nil))
                            (:has-fields (:attr board "privs") persona-id)
                            (:~ (:has-fields (:attr (:attr board "privs") persona-id) "i"))
                            (:~ (:has-fields (:attr (:attr board "privs") persona-id) "d")))))))
          (cursor (r:run sock query))
          (boards (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (alet ((boards-populated (populate-boards-data boards :get-notes get-notes :set-shared t)))
      (finish future boards-populated))))

(defafun get-board-by-id (future) (board-id &key get-notes)
  "Grab a board by id."
  (alet* ((sock (db-sock))
          ;; TODO: implement (:without ... "user_id") once >= RDB 1.8
          (query (r:r (:get (:table "boards") board-id)))
          (board (r:run sock query)))
    (r:disconnect sock)
    (if get-notes
        (alet* ((notes (get-board-notes board-id)))
          (setf (gethash "notes" board) notes)
          (finish future board)
        (finish future board)))))

(defafun add-board (future) (user-id board-data)
  "Save a board with a user."
  (setf (gethash "user_id" board-data) user-id)
  (add-id board-data)
  (add-mod board-data)
  (validate-board (board-data future)
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "boards")
                          board-data)))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future board-data))))

(defafun edit-board (future) (user-id board-id board-data)
  "Edit an existing board."
  ;; first, check if the user owns the board. any non-owner edits have to be
  ;; done via different (more specific) methods than just "LOL replace all teh
  ;; dataz immy boardt!"
  (alet ((perms (get-user-board-permissions user-id board-id)))
    (if (<= 3 perms)
        (validate-board (board-data future :edit t)
          (add-mod board-data)
          (alet* ((sock (db-sock))
                  (query (r:r (:update
                                (:get (:table "boards") board-id)
                                board-data)))
                  (nil (r:run sock query)))
            (r:disconnect sock)
            (finish future board-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a board you don't own.")))))

(defafun delete-board (future) (user-id board-id &key permanent)
  "Delete a board."
  (alet ((perms (get-user-board-permissions user-id board-id)))
    (if (<= 3 perms)
        (alet* ((sock (db-sock))
                (query (r:r (if permanent
                                (:delete (:get (:table "boards") board-id))
                                (:update
                                  (:get (:table "boards") board-id)
                                  `(("deleted" . t)
                                    ("body" . "")
                                    ("mod" . ,(get-timestamp)))))))
                (nil (r:run sock query))
                (query (r:r (if permanent
                                (:delete
                                  (:get-all (:table "notes") board-id :index "board_id"))
                                (:update
                                  (:get-all (:table "notes") board-id :index "board_id")
                                  `(("deleted" . t)
                                    ("body" . "")
                                    ("keys" . (make-hash-table))
                                    ("mod" . ,(get-timestamp)))))))
                (nil (r:run sock query)))
          (r:disconnect sock)
          (finish future t))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a board you don't own.")))))

(defafun get-user-board-permissions (future) (user/persona-id board-id)
  "Returns an integer used to determine a user/persona's permissions for the
   given board.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "boards") board-id)))
          (board (r:run sock query)))
    (r:disconnect sock)
    (if (hash-table-p board)
        (let* ((user-id (gethash "user_id" board))
               (privs (gethash "privs" board))
               (persona-privs (when (hash-table-p privs)
                                (gethash user/persona-id privs)))
               (persona-privs (when (hash-table-p persona-privs)
                                (gethash "p" persona-privs)))
               (user-privs (cond ((string= user-id user/persona-id)
                                  3)
                                 ((and (numberp persona-privs) (< 0 persona-privs))
                                  persona-privs)
                                 (t
                                  0))))
          (finish future user-privs))
        (finish future 0))))

(defafun set-board-persona-permissions (future) (user-id board-id persona-id permission-value &key invite invite-remote)
  "Gives a persona permissions to view/update a board."
  (alet ((perms (get-user-board-permissions user-id board-id))
         ;; clamp permission value to 0 <= p <= 2
         (permission-value (min (max permission-value 0) 2)))
    (if (<= 3 perms)
        (if (zerop permission-value)
            (alet ((clear-perms (clear-board-persona-permissions board-id persona-id)))
              (finish future clear-perms))
            (alet* ((sock (db-sock))
                    (priv-entry (cond (invite
                                        `(("p" . ,permission-value)
                                          ("i" . t)))
                                      (invite-remote
                                        `(("p" . ,permission-value)
                                          ("e" . ,invite-remote)))
                                      (t
                                        `(("p" . ,permission-value)))))
                    (query (r:r
                             (:update
                               (:get (:table "boards") board-id)
                               (r:fn (board)
                                 `(("privs" . ,(:merge
                                                 (:default (:attr board "privs") (make-hash-table))
                                                 `((,persona-id . ,priv-entry))))
                                   ("mod" . ,(get-timestamp)))))))
                    (nil (r:run sock query)))
              (r:disconnect sock)
              (finish future permission-value priv-entry)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a board you aren't a member of.")))))

(defafun clear-board-persona-permissions (future) (board-id persona-id &key permanent)
  "Clear out a persona's board permissions (revoke access)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:update
                     (:get (:table "boards") board-id)
                     (r:fn (board)
                       (if permanent
                           ;; permanently remove the privilege entry
                           `(("privs" . ,(:without
                                           (:attr board "privs")
                                           persona-id))
                             ("mod" . ,(get-timestamp)))
                           ;; mark the priv entry as deleted (with a ts)
                           `(("privs" . ,(:merge
                                           (:attr board "privs")
                                           `((,persona-id . (("p" . 0)
                                                             ("d" . ,(get-timestamp)))))))
                             ("mod" . ,(get-timestamp))))))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future 0)))

(defafun get-board-privs-entry (future) (board-id privs-entry-id)
  "Returns a privileges entry for the given privs entry id (usually a persona or
   invite id)."
  (alet* ((board (get-board-by-id board-id))
          (privs (gethash "privs" board))
          (entry (when privs (gethash privs-entry-id privs))))
    (finish future entry)))

(defafun add-board-remote-invite (future) (user-id board-id invite-id permission-value to-email)
  "Creates a remote (ie email) invite permission record on a board so the 
   recipient of an invite can join the board without knowing what their account
   will be in advance."
  (alet* ((email (obscure-email to-email)))
    (multiple-future-bind (perm priv-entry)
        (set-board-persona-permissions user-id board-id invite-id permission-value :invite-remote email)
      (finish future perm priv-entry))))

(defafun accept-board-invite (future) (user-id board-id persona-id &key invite-id)
  "Mark a board invitation/privilege entry as accepted. Accepts a persona, but
   also an invite-id in the event the invite was sent over email, in which case
   its record id is updated with he given persona-id."
  (with-valid-persona (persona-id user-id future)
    (alet* ((entry-id (or invite-id persona-id))
            (sock (db-sock))
            (query (r:r (:update
                          (:get (:table "boards") board-id)
                          (r:fn (board)
                            (:branch (:has-fields (:attr board "privs") entry-id)
                              ;; persona exists in this board, run the query
                              `(("privs" . ,(:merge
                                              (:without (:attr board "privs") entry-id)
                                              ;; take out the "i" key
                                              `((,persona-id . ,(:without
                                                                  (:attr (:attr board "privs") entry-id)
                                                                  "i"
                                                                  "e")))))
                                ("mod" . ,(get-timestamp)))
                              ;; Sorry, not invited
                              (:error "The persona specified is not invited to this board."))))))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future t))))

(defafun leave-board-share (future) (user-id board-id persona-id &key invite-id)
  "Allows a user who is not board owner to remove themselves from the board. If
   an invite-id is specified, it will replace the persona-id (after persona
   verification, of course) when referencing which privs entry to lear out."
  (with-valid-persona (persona-id user-id future)
    (alet ((nil (clear-board-persona-permissions board-id (or invite-id persona-id))))
      (finish future t))))


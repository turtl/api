(in-package :tagit)

(defvalidator validate-board
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("keys" :type sequence :required t :coerce simple-vector)
   ("body" :type cl-async-util:bytes-or-string)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defafun get-user-boards (future) (user-id &key get-notes get-personas)
  "Get all boards for a user, ordered by sort order."
  (alet* ((sock (db-sock))
          ;; TODO: implement (:without ... "user_id") once >= RDB 1.8
          (query (r:r 
                        (:get-all
                          (:table "boards")
                          user-id
                          :index "user_id")
                        ))
          (cursor (r:run sock query))
          (boards (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (if (and (< 0 (length boards))
             (or get-notes get-personas))
        (loop for i = 0
              for board across boards
              for board-id = (gethash "id" board) do
          (alet ((board board) ;; bind for inner form or loop will shit all over it
                 (personas (when get-personas (get-board-personas board-id)))
                 (notes (when get-notes (get-board-notes board-id))))
            (when (and get-notes notes)
              (setf (gethash "notes" board) notes))
            (when (and get-personas personas)
              (setf (gethash "personas" board) personas))
            (incf i)
            (when (<= (length boards) i)
              (finish future boards))))
        (finish future boards))))

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
                                            :msg "Sorry, you are editing a board you aren't a member of.")))))

(defafun delete-board (future) (user-id board-id)
  "Delete a board."
  (alet ((perms (get-user-board-permissions user-id board-id)))
    (if (<= 3 perms)
        (alet* ((sock (db-sock))
                (query (r:r (:delete
                              (:filter
                                (:table "boards")
                                `(("id" . ,board-id)
                                  ("user_id" . ,user-id))))))
                (res (r:run sock query)))
          (alet* ((query (r:r (:delete
                                (:filter
                                  (:table "notes")
                                  `(("board_id" . ,board-id))))))
                  (res (r:run sock query)))
            (r:disconnect sock)
            (finish future t)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a board you aren't the owner of.")))))

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

(defafun set-board-persona-permissions (future) (user-id board-id persona-id permission-value)
  "Gives a persona permissions to view/update a board."
  (alet ((perms (get-user-board-permissions user-id board-id))
         ;; clamp permission value to 0 <= p <= 2
         (permission-value (min (max permission-value 0) 2)))
    (if (<= 3 perms)
        (if (zerop permission-value)
            (alet ((clear-perms (clear-board-persona-permissions board-id persona-id)))
              (finish future clear-perms))
            (alet* ((sock (db-sock))
                    (query (r:r
                             (:update
                               (:get (:table "boards") "517ecd09735ca40fac00000c")
                               (r:fn (board)
                                 `(("privs" . ,(:merge
                                                 (:default (:attr board "privs") (make-hash-table))
                                                 `(("51dcaf26735ca406dc000009" . (("p" . 2))))))
                                   ("mod" . ,(get-timestamp)))))))
                    (nil (r:run sock query)))
              (r:disconnect sock)
              (finish future permission-value)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a board you aren't a member of.")))))

(defafun clear-board-persona-permissions (future) (board-id persona-id)
  "Clear out a persona's board permissions (revoke access)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:update
                     (:get (:table "boards") board-id)
                     (r:fn (board)
                       `(("privs" . ,(:without
                                       (:attr board "privs")
                                       persona-id))
                         ("mod" . ,(get-timestamp)))))))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future 0)))

(defafun leave-board-share (future) (board-id persona-id challenge-response)
  "Allows a user who is not board owner to remove themselves from the board."
  (aif (persona-challenge-response-valid-p persona-id challenge-response)
       (alet ((nil (clear-board-persona-permissions board-id persona-id)))
         (finish future t))
       (signal-error future (make-instance 'insufficient-privileges
                                           :msg "Sorry, either the persona you are changing board permissions for doesn't exist or you don't have access to it."))))


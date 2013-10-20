(in-package :turtl)

(defvalidator validate-note
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("board_id" :type string :required t :length 24)
   ("keys" :type sequence :required t :coerce simple-vector)
   ("body" :type cl-async-util:bytes-or-string)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defafun get-note-by-id (future) (note-id)
  "Get a note by id."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "notes") note-id)))
          (note (r:run sock query)))
    (r:disconnect sock)
    (finish future note)))

(defafun get-board-notes (future) (board-id)
  "Get the notes for a board."
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "notes")
                        board-id
                        :index "board_id")))
          (cursor (r:run sock query))
          (results (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future results)))

(defafun get-notes-from-board-ids (future) (board-ids)
  "Given a list (not vector!) of board_ids, get all notes in those boards. This
   function does no validation, so be sure you only pass it board_ids you know
   the user owns or has been shared with."
  (unless board-ids
    (finish future #())
    (return-from get-notes-from-board-ids))
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "notes")
                        board-ids
                        :index "board_id")))
          (cursor (r:run sock query))
          (results (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future results)))

(defafun get-user-note-permissions (future) (user-id note-id)
  "'Returns' an integer used to determine a user's permissions for the given
   note.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((note (get-note-by-id note-id))
          (board-perms (get-user-board-permissions user-id (gethash "board_id" note)))
          (sock (db-sock))
          (query (r:r (:== (:attr (:get (:table "notes") note-id) "user_id") user-id)))
          (note-owner-p (r:run sock query)))
    (r:disconnect sock)
    (finish future (if note-owner-p
                       3
                       board-perms))))

(defafun add-note (future) (user-id board-id note-data &key persona-id)
  "Add a new note."
  (setf (gethash "user_id" note-data) user-id
        (gethash "board_id" note-data) board-id)
  (add-id note-data)
  ;; first, check that the user/persona is a member of this board
  (alet ((perms (get-user-board-permissions (if persona-id persona-id user-id) board-id)))
    (if (<= 2 perms)
        (let ((cid (gethash "cid" note-data)))
          (validate-note (note-data future)
            (alet* ((sock (db-sock))
                    (query (r:r (:insert
                                  (:table "notes")
                                  note-data)))
                    (nil (r:run sock query))
                    (sync-ids (add-sync-record user-id
                                               "note"
                                               (gethash "id" note-data)
                                               "add"
                                               :client-id cid)))
              (r:disconnect sock)
              (setf (gethash "sync_ids" note-data) sync-ids)
              (finish future note-data))))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you aren't a member of that board.")))))

(defafun edit-note (future) (user-id note-id note-data)
  "Edit an existing note."
  ;; first, check if the user owns the note
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (validate-note (note-data future :edit t)
          (remhash "user_id" note-data)
          (alet* ((sock (db-sock))
                  (query (r:r (:update
                                (:get (:table "notes") note-id)
                                note-data)))
                  (nil (r:run sock query))
                  (sync-ids (add-sync-record user-id "note" note-id "edit")))
            (r:disconnect sock)
            (setf (gethash "sync_ids" note-data) sync-ids)
            (finish future note-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a note you don't have access to.")))))

(defafun delete-note (future) (user-id note-id)
  "Delete a note."
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (alet* ((sock (db-sock))
                (query (r:r (:delete (:get (:table "notes") note-id))))
                (nil (r:run sock query))
                (sync-ids (add-sync-record user-id "note" note-id "delete")))
          (r:disconnect sock)
          (finish future sync-ids))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a note you don't have access to.")))))

(defafun batch-note-edit (future) (user-id batch-edit-data)
  "Takes an array of note edits and invidually calls edit-note on each edit.
   Stops in its tracks if an error occurs...good thing it's idempotent."
  (loop for note-edit across batch-edit-data do
    (let ((note-id (gethash "id" note-edit)))
      (edit-note user-id note-id note-edit)))
  (finish future t))


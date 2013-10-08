(in-package :turtl)

(defvalidator validate-note
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("board_id" :type string :required t :length 24)
   ("file_id" :type string :length 24)
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
          (query (r:r (:filter
                        ;; get all user notes
                        (:get-all
                          (:table "notes")
                          board-id
                          :index "board_id")
                        (r:fn (note) (:== (:default (:attr note "deleted") nil) nil)))))
          (cursor (r:run sock query))
          (results (r:to-array sock cursor)))
    (wait-for (r:stop sock cursor)
      (r:disconnect sock))
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
  (add-mod note-data)
  ;; first, check that the user/persona is a member of this board
  (alet ((perms (get-user-board-permissions (if persona-id persona-id user-id) board-id)))
    (if (<= 2 perms)
        (validate-note (note-data future)
          (alet* ((sock (db-sock))
                  (query (r:r (:insert
                                (:table "notes")
                                note-data)))
                  (nil (r:run sock query)))
            (r:disconnect sock)
            (finish future note-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you aren't a member of that board.")))))

(defafun edit-note (future) (user-id note-id note-data)
  "Edit an existing note."
  ;; first, check if the user owns the note
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (validate-note (note-data future :edit t)
          (add-mod note-data)
          ;; don't allow ownership change or file id change
          (remhash "user_id" note-data)
          (remhash "file_id" note-data)
          (alet* ((sock (db-sock))
                  (query (r:r (:update
                                (:get (:table "notes") note-id)
                                note-data)))
                  (nil (r:run sock query))
                  (query (r:r (:get (:table "notes") note-id)))
                  (note-data (r:run sock query)))
            (r:disconnect sock)
            (finish future note-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a note you don't have access to.")))))

(defafun delete-note-file (future) (user-id note-id &key perms)
  "Delete the file attachment for a note (also removes the file itself from the
   storage system, wiping the file out forever)."
  (finish future t))

(defafun delete-note (future) (user-id note-id &key permanent)
  "Delete a note."
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (alet* ((nil (delete-note-file user-id note-id :perms perms))
                (sock (db-sock))
                (query (r:r (if permanent
                                (:delete
                                  (:filter
                                    (:table "notes")
                                    `(("id" . ,note-id)
                                      ("user_id" . ,user-id))))
                                (:update
                                  (:get (:table "notes") note-id)
                                  `(("deleted" . t)
                                    ("body" . "")
                                    ("keys" . (make-hash-table))
                                    ("mod" . ,(get-timestamp)))))))
                (res (r:run sock query)))
          (r:disconnect sock)
          (if (gethash "first_error" res)
              (signal-error future (make-instance 'server-error
                                                  :msg "There was an error deleting your note. Please try again."))
              (finish future t)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a note you don't have access to.")))))

(defafun batch-note-edit (future) (user-id batch-edit-data)
  "Takes an array of note edits and invidually calls edit-note on each edit.
   Stops in its tracks if an error occurs...good thing it's idempotent."
  (loop for note-edit across batch-edit-data do
    (let ((note-id (gethash "id" note-edit)))
      (edit-note user-id note-id note-edit)))
  (finish future t))


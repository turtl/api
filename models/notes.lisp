(in-package :turtl)

(defvalidator validate-note-file
  (("hash" :type string :required t)
   ("size" :type integer)
   ("body" :type cl-async-util:bytes-or-string)
   ("upload_id" :type string)))

(defvalidator validate-note
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("board_id" :type string :required t :length 24)
   ("file" :validator validate-note-file)
   ("keys" :type sequence :required t :coerce simple-vector)
   ("body" :type cl-async-util:bytes-or-string)
   ("mod" :type integer)))

(defafun get-note-by-id (future) (note-id)
  "Get a note by id."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "notes") note-id)))
          (note (r:run sock query)))
    (r:disconnect sock)
    (finish future note)))

(defafun get-note-board-id (future) (note-id)
  "Get a note's board id."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:attr
                     (:get-all (:table "notes") note-id)
                     "board_id")))
          (cursor (r:run sock query))
          (board-id (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future (aref board-id 0))))

(defafun get-board-notes (future) (board-id)
  "Get the notes for a board."
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "notes")
                        board-id
                        :index (db-index "notes" "board_id"))))
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
                        :index (db-index "notes" "board_id"))))
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
  (alet* ((board-id (get-note-board-id note-id))
          (board-perms (get-user-board-permissions user-id board-id))
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
        (let ((cid (gethash "cid" note-data)))
          (validate-note (note-data future)
            (alet* ((sock (db-sock))
                    (query (r:r (:insert
                                  (:table "notes")
                                  note-data)))
                    (nil (r:run sock query))
                    (user-ids (get-affected-users-from-board-ids (list (gethash "board_id" note-data))))
                    (sync-ids (add-sync-record user-id
                                               "note"
                                               (gethash "id" note-data)
                                               "add"
                                               :client-id cid
                                               :rel-ids user-ids)))
              (r:disconnect sock)
              (setf (gethash "sync_ids" note-data) sync-ids)
              (finish future note-data))))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you aren't a member of that board.")))))

(defafun edit-note (future) (user-id note-id note-data)
  "Edit an existing note."
  ;; first, check if the user owns the note, or at least has write access to the
  ;; board the note belongs to. we also check that, if moving the note to a new
  ;; board, that the user has access tot he new board as well.
  (alet* ((cur-board-id (get-note-board-id note-id))
          (new-board-id (gethash "board_id" note-data))
          (perms-cur (get-user-note-permissions user-id note-id))
          (perms-new (if (string= cur-board-id new-board-id)
                         perms-cur
                         (get-user-board-permissions user-id new-board-id))))
    (if (and (<= 2 perms-cur)
             (<= 2 perms-new))
        ;; TODO: validate if changing board_id that user is member of new board
        (validate-note (note-data future :edit t)
          (add-mod note-data)
          (remhash "user_id" note-data)
          ;; don't let a regular note edit mess with file hashes, since it will
          ;; throw syncing off.
          (alet* ((cur-board-id (get-note-board-id note-id))
                  (new-board-id (or (gethash "board_id" note-data) cur-board-id))
                  (sock (db-sock))
                  (query (r:r (:update
                                (:get (:table "notes") note-id)
                                note-data)))
                  (nil (r:run sock query))
                  ;; return the latest, full version of the note data
                  (note-data (get-note-by-id note-id))
                  (user-ids (get-affected-users-from-board-ids (list cur-board-id new-board-id)))
                  (sync-ids (add-sync-record user-id "note" note-id "edit" :rel-ids user-ids)))
            (r:disconnect sock)
            (setf (gethash "sync_ids" note-data) sync-ids)
            (finish future note-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg (if (< perms-cur 2)
                                                     "Sorry, you are editing a note you don't have access to."
                                                     "You do not have access to the board you're moving this note to."))))))

(defun make-note-file (&key hash)
  "Make a file descriptor hash object."
  (let ((filedata (make-hash-table :test #'equal)))
    (when hash (setf (gethash "hash" filedata) hash))
    filedata))

(defun get-file-path (file-id)
  "Generate the path of a file in the storage system based on its ID."
  (if *local-upload*
      (format nil "~a/~a" *local-upload* file-id)
      (format nil "/files/~a" file-id)))

(defafun get-note-file-url (future) (user-id note-id hash &key (lifetime 60))
  "Get a note's file URL. If note has no file, return nil. By default, the URL
   returned expires in 10 seconds, which should genreally be sufficient
   (especially for a redirect)."
  (alet* ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 1 perms)
        (alet* ((note (get-note-by-id note-id))
                (file (gethash "file" note)))
          (finish future
                  (when (and file (gethash "hash" file)
                             (or (not hash)
                                 (and hash (string= hash (gethash "hash" file)))))
                    (if *local-upload*
                        (format nil "~a/files/~a" *local-upload-url* note-id)
                        (get-s3-auth-url (getf *amazon-s3* :bucket)
                                         (get-file-path note-id)
                                         lifetime)))))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are accessing a note you don't have access to.")))))

(defafun edit-note-file (future) (user-id note-id file-data &key remove-upload-id skip-sync)
  "Edit a note's file data."
  (alet* ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (validate-note-file (file-data future)
          (alet* ((note (get-note-by-id note-id))
                  (board-id (gethash "board_id" note))
                  (sock (db-sock))
                  (query (r:r (:replace
                                (:get (:table "notes") note-id)
                                (r:fn (note)
                                  (:merge
                                    (:without note "file")
                                    `(("file" . ,(:literal
                                                   (:merge
                                                     (if remove-upload-id
                                                         (:without (:attr note "file") "upload_id")
                                                         (:attr note "file"))
                                                     file-data)))))))))
                  (nil (r:run sock query))
                  (user-ids (unless skip-sync (get-affected-users-from-board-ids (list board-id))))
                  (action (if (and (gethash "file" note)
                                   (gethash "hash" (gethash "file" note)))
                              "edit"
                              "add"))
                  (sync-ids (unless skip-sync (add-sync-record user-id "file" note-id action :rel-ids user-ids))))
            (r:disconnect sock)
            (setf (gethash "sync_ids" file-data) sync-ids)
            (finish future file-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a note you don't have access to.")))))

(defafun delete-note-file (future) (user-id note-id &key perms)
  "Delete the file attachment for a note (also removes the file itself from the
   storage system, wiping the file out forever)."
  (alet* ((perms (or perms (get-user-note-permissions user-id note-id))))
    (if (<= 2 perms)
        (alet* ((note (get-note-by-id note-id))
                (board-id (gethash "board_id" note)))
          (if (and (gethash "file" note)
                   (gethash "hash" (gethash "file" note)))
              (multiple-future-bind (nil res)
                  (s3-op :delete (format nil "/files/~a" note-id))
                (if (<= 200 res 299)
                    (alet* ((sock (db-sock))
                            (query (r:r (:replace
                                          (:get (:table "notes") note-id)
                                          (r:fn (note)
                                            (:without note "file")))))
                            (nil (r:run sock query))
                            (user-ids (get-affected-users-from-board-ids (list board-id)))
                            (sync-ids (add-sync-record user-id "file" note-id "delete" :rel-ids user-ids)))
                      (r:disconnect sock)
                      (finish future sync-ids))
                    (signal-error future (make-instance 'server-error
                                                        :msg "There was a problem removing that note's attachment."))))
              (finish future nil)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a note you don't have access to.")))))

(defafun delete-note (future) (user-id note-id)
  "Delete a note."
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (alet* ((file-sync-ids (delete-note-file user-id note-id :perms perms))
                (board-id (get-note-board-id note-id))
                (user-ids (get-affected-users-from-board-ids (list board-id)))
                (sock (db-sock))
                (query (r:r (:delete (:get (:table "notes") note-id))))
                (nil (r:run sock query))
                (sync-ids (add-sync-record user-id "note" note-id "delete" :rel-ids user-ids)))
          (r:disconnect sock)
          (finish future (append sync-ids file-sync-ids)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a note you don't have access to.")))))

(defafun batch-note-edit (future) (user-id batch-edit-data)
  "Takes an array of note edits and invidually calls edit-note on each edit.
   Stops in its tracks if an error occurs...good thing it's idempotent."
  (loop for note-edit across batch-edit-data do
    (let ((note-id (gethash "id" note-edit)))
      (edit-note user-id note-id note-edit)))
  (finish future t))


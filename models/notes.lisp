(in-package :turtl)

(defvalidator validate-note-file
  (("id" :type id :required t)
   ("size" :type integer)
   ("body" :type cl-async-util:bytes-or-string)
   ("upload_id" :type string))
  :old t)

(defvalidator validate-note
  (("id" :type id :required t)
   ("user_id" :type id :required t)
   ("boards" :type array :required t :coerce simple-vector)
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

(adefun note-exists-p (note-id)
  "Does this note exist?"
  (alet* ((sock (db-sock))
          (query (r:r (:pluck (:get (:table "notes") note-id) "id")))
          (note (r:run sock query)))
    (r:disconnect sock)
    note))

(defafun get-board-notes (future) (board-id)
  "Get the notes for a board."
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "notes")
                        board-id
                        :index (db-index "notes" "boards"))))
          (cursor (r:run sock query))
          (results (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future results)))

(adefun get-notes-from-board-ids (board-ids)
  "Given a list (not vector!) of board_ids, get all notes in those boards. This
   function does no validation, so be sure you only pass it board_ids you know
   the user owns or has been shared with."
  (unless board-ids
    (return-from get-notes-from-board-ids #()))
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "notes")
                        board-ids
                        :index (db-index "notes" "boards"))))
          (cursor (r:run sock query))
          (results (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    results))

(adefun get-notes-for-user (user-id)
  "Get all notes directly owned by a user."
  (alet* ((sock (db-sock))
          (query (r:r (:get-all
                        (:table "notes")
                        user-id
                        :index (db-index "notes" "user_id"))))
          (cursor (r:run sock query))
          (results (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    results))

(adefun get-all-notes (user-id board-ids)
  "Given a user ID and a set of board ids, grab all notes."
  (alet ((board-notes (get-notes-from-board-ids board-ids))
         (user-notes (get-notes-for-user user-id))
         (idx (hash))
         (final nil))
    (flet ((do-index (notes)
             (loop for note across notes
                   for id = (gethash "id" note) do
               ;; remove dupes
               (unless (gethash id idx)
                 (setf (gethash id idx) t)
                 (push note final)))))
      (do-index user-notes)
      (do-index board-notes))
    (coerce (reverse final) 'simple-vector)))

#|
(defafun get-user-note-permissions (future) (user-id note-id)
  "'Returns' an integer used to determine a user's permissions for the given
   note.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((board-id (get-note-board-id note-id)))
    (if board-id
        (alet* ((board-perms (get-user-board-permissions user-id board-id))
                (sock (db-sock))
                (query (r:r (:== (:attr (:get (:table "notes") note-id) "user_id") user-id)))
                (note-owner-p (r:run sock query)))
          (r:disconnect sock)
          (finish future (if note-owner-p
                             3
                             board-perms)))
        (signal-error future (make-instance 'not-found
                                            :msg "That note wasn't found.")))))
|#

(defun note-boards-diff (old-boards new-boards)
  "Given an old set of board IDs, a new set of board IDs, and a collection of
   board permissions a user has access to, determine the diff of the old -> new
   board ids based on the given permissions."
  (let ((old-boards (sort (copy-list old-boards) 'string<))
        (new-boards (sort (copy-list new-boards) 'string<))
        (diff nil))
    (loop
      (let ((old (car old-boards))
            (new (car new-boards)))
        (cond ((and old new)
               (cond ((string< old new)
                      (push (cons :remove old) diff)
                      (setf old-boards (cdr old-boards)))
                     ((string> old new)
                      (push (cons :add new) diff)
                      (setf new-boards (cdr new-boards)))
                     (t
                      (setf old-boards (cdr old-boards))
                      (setf new-boards (cdr new-boards)))))
              (old
               (push (cons :remove old) diff)
               (setf old-boards (cdr old-boards)))
              (new
               (push (cons :add new) diff)
               (setf new-boards (cdr new-boards)))
              (t (return)))))
    (reverse diff)))

(defun validate-diff (user-id note-owner-id diff board-perms)
  "Given a diff list and a set of permissions, make sure the diff entries are
   legit."
  ;; remove diff entries for boards we don't have access to
  (remove-if-not
    (lambda (entry)
      (let* ((board-id (cdr entry))
             (action (car entry))
             (board-perm (or (gethash board-id board-perms)
                             (hash)))
             ;; set up our permissions
             (has-board-write-perms-p (<= 2 (or (gethash "perms" board-perm) 0)))
             (user-owns-note-p (string= user-id note-owner-id))
             (user-owns-board-p (string= user-id (or (gethash "owner" board-perm) ""))))
        (declare (ignore action))
        ;(or (and user-owns-note-p
        ;         user-owns-board-p)
        ;    (and user-owns-note-p
        ;         (or (eq action :remove)
        ;             has-board-write-perms-p))
        ;    (and (eq action :remove)
        ;         user-owns-board-p))

        ;; let's make this simple for now.
        (or user-owns-note-p
            user-owns-board-p
            has-board-write-perms-p)))
    diff))

(defun apply-diff (items diff)
  "Given a set of items, apply a diff to them and return the result."
  (let ((removes (mapcar (lambda (entry) (cdr entry))
                         (remove-if-not (lambda (entry) (eq (car entry) :remove))
                                        diff)))
        (adds (mapcar (lambda (entry) (cdr entry))
                      (remove-if-not (lambda (entry) (eq (car entry) :add))
                                     diff))))
    (append
      (remove-if (lambda (id) (find id removes :test 'string=))
                 items)
      adds)))

(defun user-can-read-note-p (user-id note-data board-perms)
  "Given a set of ownership/permissions values, dtermine if a user has access to
   edit a note."
  (or (user-can-edit-note-p user-id note-data board-perms)
    ;; the user has write access to a board the note is in
    (not (zerop (length (remove-if (lambda (board-id)
                                     (let ((perm (gethash board-id board-perms)))
                                       (or (not perm)
                                           (< (gethash "perms" perm) 1))))
                                   (gethash "boards" note-data)))))))

(defun user-can-edit-note-p (user-id note-data board-perms)
  "Given a set of ownership/permissions values, dtermine if a user has access to
   edit a note."
  (or
    ;; the user owns the note. no brainer
    (string= user-id (gethash "user_id" note-data))
    ;; the user has write access to a board the note is in
    (not (zerop (length (remove-if (lambda (board-id)
                                     (let ((perm (gethash board-id board-perms)))
                                       (or (not perm)
                                           (< (gethash "perms" perm) 2))))
                                   (gethash "boards" note-data)))))))

(adefun add-note (user-id note-data)
  "Add a new note."
  (setf (gethash "user_id" note-data) user-id)
  (add-mod note-data)
  ;; first, check that the user/persona is a member of this board
  (alet* ((board-ids (gethash "boards" note-data))
          (board-perms (get-user-board-perms user-id :min-perms 2))
          (diff (map 'list (lambda (id) (cons :add id)) board-ids))
          ;; silently remove boards we don't have access to
          (diff (validate-diff user-id user-id diff board-perms))
          (board-ids (apply-diff nil diff)))
    (setf (gethash "boards" note-data) (coerce board-ids 'simple-array))
    (validate-note (note-data)
      (when (and (gethash "file" note-data)
                 (gethash "id" (gethash "file" note-data)))
        (setf (gethash "upload_id" (gethash "file" note-data)) -1))
      (alet* ((sock (db-sock))
              (query (r:r (:insert
                            (:table "notes")
                            note-data)))
              (nil (r:run sock query))
              (user-ids (get-affected-users-from-board-ids (list (gethash "boards" note-data))))
              (user-ids (concatenate 'vector (vector user-id) user-ids))
              (sync-ids (add-sync-record user-id
                                         "note"
                                         (gethash "id" note-data)
                                         "add"
                                         :rel-ids user-ids)))
        (r:disconnect sock)
        (setf (gethash "sync_ids" note-data) sync-ids)
        note-data))))

(adefun edit-note (user-id note-id note-data)
  "Edit an existing note."
  ;; first, check if the user owns the note, or at least has write access to the
  ;; board the note belongs to. we also check that, if moving the note to a new
  ;; board, that the user has access tot he new board as well.
  (alet* ((cur-note-data (get-note-by-id note-id))
          (note-user-id (gethash "user_id" cur-note-data))
          (old-board-ids (gethash "boards" cur-note-data))
          (new-board-ids (gethash "boards" note-data))
          (board-perms (get-user-board-perms user-id :min-perms 2))
          (diff (note-boards-diff old-board-ids new-board-ids))
          ;; silently remove boards we don't have access to
          (diff (validate-diff user-id note-user-id diff board-perms))
          (board-ids (apply-diff old-board-ids diff)))
    (unless (user-can-edit-note-p user-id cur-note-data board-perms)
      (error 'insufficient-privileges
             :msg "Sorry, you are editing a note you don't have access to."))
    (setf (gethash "boards" note-data) (coerce board-ids 'simple-array))
    (validate-note (note-data)
      (add-mod note-data)
      ;; don't allow changing the note user
      (setf (gethash "user_id" note-data) (gethash "user_id" cur-note-data))
      (alet* ((sock (db-sock))
              (query (r:r (:replace
                            (:get (:table "notes") note-id)
                            note-data)))
              (nil (r:run sock query))
              ;; return the latest, full version of the note data
              (note-data (get-note-by-id note-id))
              (user-ids (get-affected-users-from-board-ids (append old-board-ids new-board-ids)))
              (sync-ids (add-sync-record user-id "note" note-id "edit" :rel-ids user-ids)))
        (r:disconnect sock)
        (setf (gethash "sync_ids" note-data) sync-ids)
        note-data))))

(defun get-file-path (file-id)
  "Generate the path of a file in the storage system based on its ID."
  (if *local-upload*
      (format nil "~a/~a" *local-upload* file-id)
      (format nil "/files/~a" file-id)))

(defafun get-note-file-url (future) (user-id note-id &key (lifetime 60))
  "Get a note's file URL. If note has no file, return nil. By default, the URL
   returned expires in 10 seconds, which should genreally be sufficient
   (especially for a redirect)."
  (alet* ((note (get-note-by-id note-id))
          (board-perms (get-user-board-perms user-id :min-perms 1)))
    (if note
        (if (user-can-read-note-p user-id note board-perms)
            (alet* ((file (gethash "file" note))
                    (id (gethash "id" file)))
              (finish future
                      (when (and file id)
                        (if *local-upload*
                            (format nil "~a/files/~a" *local-upload-url* note-id)
                            (get-s3-auth-url (getf *amazon-s3* :bucket)
                                             (get-file-path note-id)
                                             lifetime)))))
            (signal-error future (make-instance 'insufficient-privileges
                                                :msg "Sorry, you are accessing a note you don't have access to.")))
        (signal-error future (make-instance 'not-found :msg "That note wasn't found.")))))

(defafun edit-note-file (future) (user-id note-id file-data &key remove-upload-id skip-sync)
  "Edit a note's file data."
  (alet* ((note (get-note-by-id note-id))
          (board-perms (get-user-board-perms user-id :min-perms 2))
          (allowed (user-can-edit-note-p user-id note board-perms)))
    (if allowed
        (validate-note-file (file-data future)
          (alet* ((board-ids (gethash "boards" note))
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
                  (user-ids (unless skip-sync (get-affected-users-from-board-ids board-ids)))
                  ;; always add the file
                  (action "add")
                  (sync-ids (unless skip-sync (add-sync-record user-id "file" note-id action :rel-ids user-ids))))
            (r:disconnect sock)
            (setf (gethash "sync_ids" file-data) sync-ids)
            (finish future file-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a note you don't have access to.")))))

(adefun do-delete-note-file (user-id note-id &key note)
  "Do the legwork of note file deletion."
  (catcher
    (alet* ((note (or note (get-note-by-id note-id)))
            (board-ids (gethash "boards" note)))
      (when (and (gethash "file" note)
               (gethash "id" (gethash "file" note)))
        (multiple-promise-bind (nil res)
            (if *local-upload*
                (let ((file-path (get-file-path note-id)))
                  (values (and (probe-file file-path) (delete-file file-path)) 200))
                (s3-op :delete (format nil "/files/~a" note-id)))
          (if (<= 200 res 299)
              (alet* ((sock (db-sock))
                      (query (r:r (:replace
                                    (:get (:table "notes") note-id)
                                    (r:fn (note)
                                      (:without note "file")))))
                      (nil (r:run sock query))
                      (user-ids (get-affected-users-from-board-ids board-ids))
                      (sync-ids (add-sync-record user-id "file" note-id "delete" :rel-ids user-ids)))
                (r:disconnect sock)
                sync-ids)
              (error 'server-error :msg "There was a problem removing that note's attachment.")))))
    ;; silently fail when we're deleting a note file that doesn't exist
    (not-found () nil)))

(adefun delete-note-file (user-id note-id)
  "Delete the file attachment for a note (also removes the file itself from the
   storage system, wiping the file out forever)."
  (alet* ((note-data (get-note-by-id note-id))
          (board-perms (get-user-board-perms user-id :min-perms 2)))
    ;; skip empty notes
    (when note-data
      (unless (user-can-edit-note-p user-id note-data board-perms)
        (error 'insufficient-privileges :msg "Sorry, you are editing a note you don't have access to."))
      (do-delete-note-file user-id note-id :note note-data))))

(adefun delete-note (user-id note-id)
  "Delete a note."
  (alet* ((note-data (get-note-by-id note-id))
          (board-perms (get-user-board-perms user-id :min-perms 2)))
    (block note-data
      (unless note-data
        (return-from note-data #()))
      (unless (user-can-edit-note-p user-id note-data board-perms)
        (error 'insufficient-privileges :msg "Sorry, you are deleting a note you don't have access to."))
      (alet* ((file-sync-ids (do-delete-note-file user-id note-id :note note-data))
              (user-ids (get-affected-users-from-board-ids (gethash "boards" note-data)))
              (sock (db-sock))
              (query (r:r (:delete (:get (:table "notes") note-id))))
              (nil (r:run sock query))
              (nil (delete-keychain-entries user-id note-id))
              (sync-ids (add-sync-record user-id "note" note-id "delete" :rel-ids user-ids)))
        (r:disconnect sock)
        (append sync-ids file-sync-ids)))))

(defun get-file-size-summary (bytes)
  "Given a size in bytes, return a summary of how large the file is (used mainly
   for analytics)."
  (cond ((< bytes 1024)
         "0 - 1kb")
        ((< bytes (* 1024 10))
         "1kb - 10kb")
        ((< bytes (* 1024 100))
         "10kb - 100kb")
        ((< bytes (* 1024 1024))
         "100kb - 1mb")
        ((< bytes (* 1024 1024 10))
         "1mb - 10mb")
        ((< bytes (* 1024 1024 100))
         "10mb - 100mb")
        ((< bytes (* 1024 1024 1024))
         "100mb - 1gb")
        ((< bytes (* 1024 1024 1024 10))
         "1gb - 10gb")
        ((< bytes (* 1024 1024 1024 100))
         "10gb - 100gb")
        ((< bytes (* 1024 1024 1024 1024))
         "100gb - 1tb")
        (t "> 1tb")))


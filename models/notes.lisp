(in-package :tagit)

(defvalidator validate-note
  (("id" :type 'string :required t :length 24)
   ("user_id" :type 'string :required t :length 24)
   ("project_id" :type 'string :required t :length 24)
   ("body" :type 'cl-async-util:bytes-or-string)
   ("meta" :type 'string)
   ("sort" :type 'integer :required t :default 99999)))

(defafun get-user-notes (future) (user-id project-id)
  "Get the notes for a user/project."
  (alet* ((sock (db-sock))
          (query (r:r (:order-by
                        (:filter
                          (:table "notes")
                          `(("user_id" . ,user-id)
                            ("project_id" . ,project-id)))
                        (:asc "sort")
                        (:asc "id"))))
          (cursor (r:run sock query)))
    (alet ((results (r:to-array sock cursor)))
      (wait-for (r:stop sock cursor)
        (r:disconnect sock))
      (finish future results))))

(defafun add-note (future) (user-id project-id note-data)
  "Add a new note."
  (setf (gethash "user_id" note-data) user-id
        (gethash "project_id" note-data) project-id
        (gethash "sort" note-data) 99999)
  (add-id note-data)
  ;; first, check that the user is a member of this project
  (alet ((perms (get-user-project-permissions user-id project-id)))
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
                                            :msg "Sorry, you aren't a member of that project.")))))

(defafun edit-note (future) (user-id note-id note-data)
  "Edit an existing note."
  ;; first, check if the user owns the note
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 2 perms)
        (validate-note (note-data future :edit t)
          (alet* ((sock (db-sock))
                  (query (r:r (:update
                                (:get (:table "notes") note-id)
                                note-data)))
                  (nil (r:run sock query)))
            (r:disconnect sock)
            (finish future note-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a note you aren't a member of.")))))

(defafun delete-note (future) (user-id note-id)
  "Delete a note."
  (alet ((perms (get-user-note-permissions user-id note-id)))
    (if (<= 3 perms)
        (alet* ((sock (db-sock))
                (query (r:r (:delete
                              (:filter
                                (:table "notes")
                                `(("id" . ,note-id)
                                  ("user_id" . ,user-id))))))
                (res (r:run sock query)))
          (r:disconnect sock)
          (finish future t))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a note you aren't the owner of.")))))

(defafun get-user-note-permissions (future) (user-id note-id)
  "'Returns' an integer used to determine a user's permissions for the given
   note.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((sock (db-sock))
          (privs-query (r:r (:== (:attr (:get (:table "notes") note-id) "user_id")
                                 user-id)))
          (is-kewl (r:run sock privs-query)))
    (r:disconnect sock)
    ;; right now, you either own it or you don't...
    (finish future (if is-kewl
                       3
                       0))))


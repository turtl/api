(in-package :tagit)

(defvalidator validate-note
  (("id" :type 'string :required t :length 24)
   ("title" :type 'string)
   ("type" :type 'string :required t :values ("text" "link" "image" "embed"))
   ("body" :type 'cl-async-util:bytes-or-string)
   ("sort" :type 'integer)
   ("tags" :type 'list)))

(defun get-user-notes (user-id project-id)
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
      results)))

(defun add-note (user-id project-id note-data)
  "Add a new note."
  (setf (gethash "user_id" note-data) user-id
        (gethash "project_id" note-data) project-id)
  (add-id note-data)
  (let ((future (make-future)))
    (validate-note (note-date future)
      (alet* ((sock (db-sock))
              (query (r:r (:insert
                            (:table "notes")
                            note-data)))
              (nil (r:run sock query)))
        (r:disconnect sock)
        (finish future note-data)))
    future))

(defun delete-note (user-id note-id)
  "Delete a note."
  (let ((future (make-future)))
    (alet* ((sock (db-sock))
            (query (r:r (:get (:table "notes") note-id)))
            (project (r:run sock query)))
      (cond ((and project (string= (gethash "user_id" project) user-id))
             (alet* ((query (r:r (:delete (:get (:table "projects") project-id))))
                     (nil (r:run sock query)))
               (r:disconnect sock)
               (finish future)))
            (project
             (r:disconnect sock)
             (signal-error future (make-instance 'insufficient-priviliges
                                                 :msg "You can't delete a project you don't own.")))
            (t
             (r:disconnect sock)
             (signal-error future (make-instance 'not-found
                                                 :msg "That project wasn't found.")))))
    future))


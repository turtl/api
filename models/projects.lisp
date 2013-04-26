(in-package :tagit)

(defvalidator validate-project
  (("id" :type 'string :required t :length 24)
   ("user_id" :type 'string :required t :length 24)
   ("keys" :type 'list :required t)
   ("body" :type 'cl-async-util:bytes-or-string)
   ("sort" :type 'integer :required t :default 99999)))

(defafun get-user-projects (future) (user-id)
  "Get all projects for a user, ordered by sort order."
  (alet* ((sock (db-sock))
          (query (r:r (:order-by
                        (:filter
                          (:table "projects")
                          (r:fn (p)
                            (:== (:attr p "user_id") user-id)))
                        (:asc "sort")
                        (:asc "id"))))
          (cursor (r:run sock query)))
    (alet ((results (r:to-array sock cursor)))
      (wait-for (r:stop sock cursor)
        (r:disconnect sock)
        (finish future results)))))

(defafun add-project (future) (user-id project-data)
  "Save a project with a user."
  (setf (gethash "user_id" project-data) user-id)
  (unless (gethash "sort" project-data)
    (setf (gethash "sort" project-data) 99999))
  (add-id project-data)
  (validate-project (project-data future)
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "projects")
                          project-data)))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future project-data))))

(defafun edit-project (future) (user-id project-id project-data)
  "Edit an existing project."

  ;; TODO remove once validation is built
  (let* ((sort (gethash "sort" project-data))
         (sort (ignore-errors (parse-integer sort))))
    (when sort
      (setf (gethash "sort" project-data) sort)))

  ;; first, check if the user owns the project
  (alet ((perms (get-user-project-permissions user-id project-id)))
    (if (<= 2 perms)
        (validate-project (project-data future :edit t)
          (alet* ((sock (db-sock))
                  (query (r:r (:update
                                (:get (:table "projects") project-id)
                                project-data)))
                  (nil (r:run sock query)))
            (r:disconnect sock)
            (finish future project-data)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a project you aren't a member of.")))))

(defafun delete-project (future) (user-id project-id)
  "Delete a project."
  (alet ((perms (get-user-project-permissions user-id project-id)))
    (if (<= 3 perms)
        (alet* ((sock (db-sock))
                (query (r:r (:delete
                              (:filter
                                (:table "projects")
                                `(("id" . ,project-id)
                                  ("user_id" . ,user-id))))))
                (res (r:run sock query)))
          (alet* ((query (r:r (:delete
                                (:filter
                                  (:table "notes")
                                  `(("project_id" . ,project-id))))))
                  (res (r:run sock query)))
            (r:disconnect sock)
            (finish future t)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are deleting a project you aren't the owner of.")))))

(defafun get-user-project-permissions (future) (user-id project-id)
  "'Returns' an integer used to determine a user's permissions for the given
   project.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((sock (db-sock))
          (privs-query (r:r (:== (:attr (:get (:table "projects") project-id) "user_id")
                                 user-id)))
          (is-kewl (r:run sock privs-query)))
    (r:disconnect sock)
    ;; right now, you either own it or you don't...
    (finish future (if is-kewl
                       3
                       0))))


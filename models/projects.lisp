(in-package :tagit)

(defvalidator validate-project
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("keys" :type sequence :required t)
   ("body" :type cl-async-util:bytes-or-string)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defvalidator validate-key
  (("k" :type string :required t)
   ("a" :type string :required nil :length 24)    ;; persona ("[A]vatar")
   ("u" :type string :required nil :length 24)    ;; user
   ("p" :type string :required nil :length 24)))  ;; project

(defafun get-user-projects (future) (user-id get-notes)
  "Get all projects for a user, ordered by sort order."
  (alet* ((sock (db-sock))
          (query (r:r (:order-by
                        (:get-all
                          (:table "projects")
                          user-id
                          :index "user_id")
                        (:asc "sort")
                        (:asc "id"))))
          (cursor (r:run sock query))
          (projects (r:to-array sock cursor)))
    (if (r:cursorp cursor)
        (wait-for (r:stop sock cursor)
          (r:disconnect sock))
        (r:disconnect sock))
    (if (and get-notes
             (< 0 (length projects)))
        (loop for i = 0
              for project across projects do
          (alet ((project project) ;; bind for inner form or loop will shit all over it
                 (notes (get-user-notes user-id (gethash "id" project))))
            (setf (gethash "notes" project) notes)
            (incf i)
            (when (<= (length projects) i)
              (finish future projects))))
        (finish future projects))))

(defafun add-project (future) (user-id project-data)
  "Save a project with a user."
  (setf (gethash "user_id" project-data) user-id)
  (unless (gethash "sort" project-data)
    (setf (gethash "sort" project-data) 99999))
  (add-id project-data)
  (add-mod project-data)
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
  ;; first, check if the user owns the project
  (alet ((perms (get-user-project-permissions user-id project-id)))
    (if (<= 3 perms)
        (validate-project (project-data future :edit t)
          (add-mod project-data)
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

(defafun get-user-project-permissions (future) (user/persona-id project-id)
  "Returns an integer used to determine a user's permissions for the given
   project.
   
   0 == no permissions
   1 == read permissions
   2 == update permissions
   3 == owner"
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "projects") project-id)))
          (project (r:run sock privs-query)))
    (r:disconnect sock)
    ;; right now, you either own it or you don't...
    (if (hash-table-p project)
        (let* ((user-id (gethash "user_id" project))
               (privs (gethash "privs" project))
               (persona-privs (if (hash-table-p privs)
                                  (gethash user/persona-id privs)
                                  nil))
               (user-privs (cond ((string= user-id user/persona-id)
                                  3)
                                 ((and (numberp persona-privs) (< 0 persona-privs))
                                  persona-privs)
                                 (t
                                  0))))
          (finish future user-privs))
        (finish future 0))))

(defafun set-project-persona-permissions (future) (user-id project-id persona-id permission-value)
  "Gives a persona permissions to view/update a project."
  (alet ((perms (get-user-project-permissions user-id project-id)))
    (if (<= 3 perms)
        (if (zerop permission-value)
            (alet ((clear-perms (clear-project-persona-permissions project-id persona-id)))
              (finish future clear-perms))
              (alet* ((sock (db-sock))
                      (query (r:r
                               (:update
                                 (:get (:table "projects") project-id)
                                 `(("privs" . ,(:merge
                                                 (:row "privs")
                                                 `((,persona-id ,permission-value))))))))
                      (nil (r:run sock query)))
                (finish future permission-value)))
        (signal-error future (make-instance 'insufficient-privileges
                                            :msg "Sorry, you are editing a project you aren't a member of.")))))

(defafun clear-project-persona-permissions (future) (project-id persona-id)
  "Clear out a persona's project permissions (revoke access)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:update
                     (:get (:table "projects") project-id)
                     `(("privs" . ,(:without
                                     (:row "privs")
                                     persona-id))))))
          (nil (r:run sock query)))
    (finish future 0)))

(defafun project-add-persona-key (future) (project-id persona-id challenge-response keydata)
  "Add a persona's key to a project's key data."
  (validate-key (keydata future)
    (aif (persona-challenge-response-valid-p persona-id challenge-response)
         (alet ((perms (get-user-project-permissions user-id project-id)))
           (if (<= 1 perms)
               (alet* ((sock (db-sock))
                       (sub-query (r:r
                                    ;; if the keys array already contains {a: <persona-id>} ...
                                    (:branch (:contains
                                               (:map
                                                 (:row "keys")
                                                 (r:fn (key)
                                                   ;; :default prevents "BAD KEY" errors
                                                   (:default (:attr key "a") nil)))
                                               persona-id)
                                      ;; key already exists in project, update it (ugh)
                                      (:)
                                      ;; this is a new key, add it
                                      (:append
                                        (:row "keys")
                                        `(("a" . ,persona-id)
                                          ("k" . ,keydata))))))
                       (query (r:r
                                (:update
                                  (:get (:table "projects") project-id)
                                  `(("keys" . ,sub-query)))))
                       (nil (r:run sock query)))
                 (finish future keydata))
               (signal-error future (make-instance 'insufficient-privileges
                                                   :msg "Sorry, you are modifying a project you aren't a member of.")))))
         (signal-error future (make-instance 'insufficient-privileges
                                             :msg "Sorry, the persona you are modifying does not exist or you passed the wrong challenge response.")))))


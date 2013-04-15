(in-package :tagit)

(defvalidator validate-project
  (("id" :type 'string :required t :length 24)
   ("user_id" :type 'string :required t :length 24)
   ("name" :type 'string :required t :length (1 64))
   ("sort" :type 'integer :required t)))

(defun get-user-projects (user-id)
  "Get all projects for a user, ordered by sort order."
  (let ((future (make-future)))
    (alet* ((sock (db-sock))
            (query (r:r (:order-by
                          (:filter
                            (:table "projects")
                            (r:fn (p)
                              (:== (:attr p "user_id") user-id)))
                          (:asc "sort"))))
            (cursor (r:run sock query)))
      (alet ((results (r:to-array sock cursor)))
        (wait-for (r:stop sock cursor)
          (r:disconnect sock)
          (finish future results))))
    future))

(defun add-project (user-id project-data)
  "Save a project with a user."
  (setf (gethash "user_id" project-data) user-id)
  (unless (gethash "sort" project-data)
    (setf (gethash "sort" project-data) 99999))
  (add-id project-data)
  (let ((future (make-future)))
    (validate-project (project-data future)
      (alet* ((sock (db-sock))
              (query (r:r (:insert
                            (:table "projects")
                            project-data)))
              (nil (r:run sock query)))
        (r:disconnect sock)
        (finish future project-data)))
    future))

(defun delete-project (user-id project-id)
  "Delete a project."
  (let ((future (make-future)))
    (format t "del: ~s~%" (list user-id project-id))
    (alet* ((sock (db-sock))
            (query (r:r (:delete
                          (:filter
                            (:table "projects")
                            `(("id" . ,project-id)
                              ("user_id" . ,user-id))))))
            (res (r:run sock query)))
      (r:disconnect sock)
      (if (and (gethash res "deleted")
               (< 0 (gethash res "deleted")))
          (finish future t)
          (signal-error future (make-instance 'not-found
                                              :msg "That project wasn't found."))))
    future))


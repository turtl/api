(in-package :tagit)

(defvalidator validate-project)
              
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
  (setf (gethash "user_id" project-data) user-id
        (gethash "sort" project-data) 99999)
  ;(add-id project-data)
  (let ((future (make-future))
        (has-errors (validate-project project-data)))
    (if has-errors
        (signal-error future (make-instance 'validation-failed
                                            :msg (format nil "Bad project: ~s~%" has-errors)))
        (alet* ((sock (db-sock))
                (query (r:r (:insert
                              (:table "projects")
                              project-data)))
                (nil (r:run sock query)))
          (r:disconnect sock)
          (finish future project-data)))
    future))


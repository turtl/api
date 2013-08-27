(in-package :turtl)

(define-condition persona-email-exists (turtl-error)
  ((code :initform 403)))
  
(defvalidator validate-persona
  (("id" :type string :required t :length 24)
   ("user_id" :type string :required t :length 24)
   ("pubkey" :type string :required t)
   ("email" :type string :required t :transform string-downcase)
   ;("screenname" :type string :required t :max-length 24)
   ("name" :type string)
   ("body" :type cl-async-util:bytes-or-string :required t)
   ("mod" :type integer :required t :default 'get-timestamp)))

(defafun get-persona-by-id (future) (persona-id)
  "Get a persona by id."
  (alet* ((sock (db-sock))
          (query (r:r (:get (:table "personas") persona-id)))
          (persona (r:run sock query)))
    (r:disconnect sock)
    (finish future persona)))

(defafun get-user-personas (future) (user-id)
  "Get personas by user id."
  (alet* ((sock (db-sock))
          (query (r:r (:filter
                        (:get-all
                          (:table "personas")
                          user-id
                          :index "user_id")
                        (r:fn (persona)
                          (:~ (:default (:attr persona "deleted") nil))))))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future personas)))

(defafun user-personas-map (future) (user-id map-fn &key flatten)
  "Run a function on all of a user's persona's and collect the results as an
   array. The callback takes one argument, the persona id."
  (alet* ((personas (get-user-personas user-id))
          (items nil))
    (if (< 0 (length personas))
        (loop for i = 0
              for persona across personas
              for persona-id = (gethash "id" persona) do
          (alet ((item (funcall map-fn persona-id)))
            (if (and flatten
                     (or (typep item 'list)
                         (typep item 'array)))
                (cl-rethinkdb-util:do-list/vector (subitem item)
                  (push subitem items))
                (push item items))
            (incf i)
            (when (<= i (length personas))
              (finish future (coerce (nreverse items) 'simple-vector)))))
        (finish future #()))))
  
;; TODO: find a way to limit number of personas per account/user.
;; might not be possible with current method of obscuring the links.
(defafun add-persona (future) (user-id persona-data)
  "Add a persona to the system."
  (add-id persona-data)
  (add-mod persona-data)
  (setf (gethash "user_id" persona-data) user-id)
  (validate-persona (persona-data future)
    (aif (persona-email-available-p (gethash "email" persona-data))
         (alet* ((sock (db-sock))
                 (query (r:r (:insert
                               (:table "personas")
                               persona-data)))
                 (nil (r:run sock query)))
           (r:disconnect sock)
           (finish future persona-data))
         (signal-error future (make-instance 'persona-email-exists
                                             :msg "That email is already registered to another persona.")))))

(defafun edit-persona (future) (user-id persona-id persona-data)
  "Update a persona."
  (with-valid-persona (persona-id user-id future)
    (validate-persona (persona-data future :edit t)
      (add-mod persona-data)
      (setf (gethash "user_id" persona-data) user-id)
      (alet* ((email (gethash "email" persona-data))
              (availablep (if (or (not email)
                                  (persona-email-available-p email persona-id))
                              t
                              nil)))
        (if availablep
            (alet* ((sock (db-sock))
                    (query (r:r (:replace
                                  (:get (:table "personas") persona-id)
                                  (r:fn (persona)
                                    (:merge persona-data
                                            (:pluck persona "secret"))))))
                    (nil (r:run sock query)))
              (r:disconnect sock)
              (finish future persona-data))
            (signal-error future (make-instance 'persona-email-exists
                                                :msg "That email is taken by another persona.")))))))

(defafun delete-persona (future) (user-id persona-id &key permanent)
  "Delete a persona."
  (with-valid-persona (persona-id user-id future)
    (alet* ((sock (db-sock))
            (query (r:r (if permanent
                            (:delete (:get (:table "personas") persona-id))
                            (:update
                              (:get (:table "personas") persona-id)
                              `(("deleted" . t)
                                ("email" . "")
                                ("mod" . ,(get-timestamp)))))))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (finish future t))))

(defafun get-persona-by-email (future) (email &optional ignore-persona-id)
  "Grab a persona via its email. Must be an exact match (for now)."
  (alet* ((sock (db-sock))
          (email (string-downcase email))
          ;; TODO: implement (:without ... "secret") when Rethink is fixed
          (query (r:r (:limit
                        (:get-all (:table "personas")
                                  email
                                  :index "email")
                        1)))
          (cursor (r:run sock query))
          (persona (when (r:has-next cursor)
                     (r:next sock cursor))))
    (r:stop/disconnect sock cursor)
    (if (and (hash-table-p persona)
             (not (string= ignore-persona-id (gethash "id" persona))))
        (progn
          (remhash "secret" persona)
          (finish future persona))
        (finish future nil))))

(defafun get-board-personas (future) (board-id)
  "Given a board ID, find all personas that board is shared with and pull them
   out."
  (alet* ((sock (db-sock))
          (query (r:r (:do
                        (r:fn (board)
                          (:map
                            (:filter
                              (:keys
                                (:default
                                  (:attr board "privs")
                                  (make-hash-table)))
                              (r:fn (key)
                                (:&& (:~ (:== (:attr (:attr (:attr board "privs") key) "p") 0))
                                     (:~ (:default (:attr (:attr (:attr board "privs") key) "d") nil)))))
                            (r:fn (pid)
                              (:get (:table "personas") pid))))
                        (:get (:table "boards") board-id))))
          (personas (r:run sock query)))
    (r:disconnect sock)
    (finish future (remove nil personas))))

(defafun persona-email-available-p (future) (email &optional ignore-id)
  "Test whether or not a email is available."
  (aif (get-persona-by-email email ignore-id)
       (finish future nil)
       (finish future t)))

(defafun persona-owned-by-user-p (future) (persona-id user-id)
  "Determines if a user is the owner of a persona."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:default (:get (:table "personas") persona-id) (make-hash-table))
                        "user_id")))
          (persona (r:run sock query))
          (validp (string= user-id (gethash "user_id" persona))))
    (r:disconnect sock)
    (finish future validp)))

;; NOTE: this function is not used and won't be until personas offer account
;; obscuration again.
(defafun persona-challenge-response-valid-p (future) (persona-id response)
  "Determine if the given response if valid for the persona specified by
   persona-id."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:default (:get (:table "personas") persona-id) (make-hash-table))
                        "secret")))
          (persona (r:run sock query))
          (validp (verify-challenge :persona persona-id (gethash "secret" persona) response)))
    (r:disconnect sock)
    (finish future validp)))


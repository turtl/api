(in-package :tagit)

(define-condition persona-email-exists (tagit-error)
  ((code :initform 403)))
  
(defvalidator validate-persona
  (("id" :type string :required t :length 24)
   ("secret" :type string :required t)
   ("pubkey" :type string :required t)
   ("email" :type string :required t :transform string-downcase)
   ;("screenname" :type string :required t :max-length 24)
   ("name" :type string)
   ("body" :type cl-async-util:bytes-or-string :required t)))

(defafun get-persona-by-id (future) (persona-id)
  "Get a persona by id."
  (alet* ((sock (db-sock))
          (query (r:r (:without
                        (:default
                          (:get (:table "personas") persona-id)
                          #())
                        "secret")))
          (persona (r:run sock query)))
    (r:disconnect sock)
    (finish future persona)))

(defafun search-personas (future) (&key email)
  "Search personas by various criteria."
  (alet* ((sock (db-sock))
          (email (string-downcase email))
          (query (r:r (:limit
                        (:filter
                          (:table "personas")
                          (r:fn (p)
                            (:match (:attr p "email")
                                    (concatenate 'string "^" email))))
                        10)))
          (cursor (r:run sock query))
          (personas (r:to-array sock cursor)))
    (r:stop/disconnect sock cursor)
    (finish future personas)))

;; TODO: find a way to limit number of personas per account/user.
;; might not be possible with current method of obscuring the links.
(defafun add-persona (future) (secret persona-data)
  "Add a persona to the system."
  (setf (gethash "secret" persona-data) secret)
  (add-id persona-data)
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

(defafun edit-persona (future) (persona-id challenge-response persona-data)
  "Update a persona. Validates the passed challenge-response."
  (with-valid-persona (persona-id challenge-response future)
    (validate-persona (persona-data future :edit t)
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

(defafun delete-persona (future) (persona-id challenge-response)
  "Delete a persona. Validates the passed challenge-response."
  (with-valid-persona (persona-id challenge-response future)
    (alet* ((sock (db-sock))
            (query (r:r (:delete (:get (:table "personas") persona-id))))
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
    (finish future personas)))

(defafun persona-email-available-p (future) (email &optional ignore-id)
  "Test whether or not a email is available."
  (aif (get-persona-by-email email ignore-id)
       (finish future nil)
       (finish future t)))

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


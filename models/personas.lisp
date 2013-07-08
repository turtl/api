(in-package :tagit)

(define-condition persona-screenname-exists (tagit-error)
  ((code :initform 403)))
  
(defvalidator validate-persona
  (("id" :type string :required t :length 24)
   ("secret" :type string :required t)
   ("pubkey" :type string :required t)
   ("screenname" :type string :required t)
   ("name" :type string)
   ("email" :type string)
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

;; TODO: find a way to limit number of personas per account/user.
;; might not be possible with current method of obscuring the links.
(defafun add-persona (future) (secret persona-data)
  "Add a persona to the system."
  (setf (gethash "secret" persona-data) secret)
  (add-id persona-data)
  (validate-persona (persona-data future)
    (aif (persona-screenname-available-p (gethash "screenname" persona-data))
         (alet* ((sock (db-sock))
                 (query (r:r (:insert
                               (:table "personas")
                               persona-data)))
                 (nil (r:run sock query)))
           (r:disconnect sock)
           (finish future persona-data))
         (signal-error future (make-instance 'persona-screenname-exists
                                             :msg "That screenname is taken.")))))

(defafun edit-persona (future) (persona-id challenge-response persona-data)
  "Update a persona. Validates the passed challenge-response."
  (aif (persona-challenge-response-valid-p persona-id challenge-response)
       (validate-persona (persona-data future :edit t)
         (alet* ((screenname (gethash "screenname" persona-data))
                 (availablep (if (or (not screenname)
                                     (persona-screenname-available-p screenname persona-id))
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
               (signal-error future (make-instance 'persona-screenname-exists
                                                   :msg "That screenname is taken.")))))
       (signal-error future (make-instance 'insufficient-privileges
                                           :msg "Sorry, the persona you are modifying does not exist or you passed the wrong challenge response."))))

(defafun delete-persona (future) (persona-id challenge-response)
  "Delete a persona. Validates the passed challenge-response."
  (aif (persona-challenge-response-valid-p persona-id challenge-response)
       (alet* ((sock (db-sock))
               (query (r:r (:delete (:get (:table "personas") persona-id))))
               (nil (r:run sock query)))
         (r:disconnect sock)
         (finish future t))
       (signal-error future (make-instance 'insufficient-privileges
                                           :msg "Sorry, the persona you are modifying does not exist or you passed the wrong challenge response."))))

(defafun get-persona-by-screenname (future) (screenname &optional ignore-persona-id)
  "Grab a persona via its screenname. Must be an exact match (for now)."
  (alet* ((sock (db-sock))
          (query (r:r (:limit
                        (:without
                          (:get-all (:table "personas")
                                    screenname
                                    :index "screenname")
                          "secret")
                        1)))
          (cursor (r:run sock query))
          (persona (when (r:has-next cursor)
                     (r:next sock cursor))))
    (if (r:cursorp cursor)
        (wait-for (r:stop sock cursor)
          (r:disconnect sock))
        (r:disconnect sock))
    (if (and (hash-table-p persona)
             (not (string= ignore-persona-id (gethash "id" persona))))
        (finish future persona)
        (finish future nil))))

(defafun persona-screenname-available-p (future) (screenname &optional ignore-id)
  "Test whether or not a screenname is available."
  (aif (get-persona-by-screenname screenname ignore-id)
       (finish future nil)
       (finish future t)))

(defafun persona-challenge-response-valid-p (future) (persona-id response)
  "Determine if the given response if valid for the persona specified by
   persona-id."
  (alet* ((sock (db-sock))
          (query (r:r (:pluck
                        (:get (:table "personas") persona-id)
                        "secret")))
          (persona (r:run sock query))
          (validp (verify-challenge :persona persona-id (gethash "secret" persona) response)))
    (r:disconnect sock)
    (finish future validp)))


(in-package :tagit)

(defvalidator validate-message
  (("id" :type string :required t :length 24)
   ("from" :type string :required t :length 24)
   ("to" :type string :required t :length 24)
   ("keys" :type sequence :required t)
   ("data" :type cl-async-util:bytes-or-string)))

(defafun get-messages-by-persona (future) (persona-id &key (after "") (index "get_messages_to"))
  "Gets messages for a persona. If a message ID is specified for :after, will
   only get messages after that ID."
  (alet* ((sock (db-sock))
          (query (r:r (:map
                        (:eq-join
                          (:between
                            (:table "messages")
                            :left (list persona-id (concatenate 'string after ".")) ;; moar hax
                            :right (list persona-id "zzzzzzzzzzzzzzzzzzzzzzzzz")    ;; lol h4x
                            :index index)
                          "from"
                          (:table "personas"))
                        (r:fn (row)
                          (:without
                            (:merge
                              (:merge
                                row
                                (:attr row "left"))
                              `(("persona" . ,(:without (:attr row "right") "secret"))))
                            "left"
                            "right")))))
          (cursor (r:run sock query))
          (res (r:to-array sock cursor)))
    (if (r:cursorp cursor)
        (wait-for (r:stop sock cursor)
          (r:disconnect sock))
        (r:disconnect sock))
    (finish future res)))

(defafun get-messages-for-persona (future) (persona-id challenge-response &key (after ""))
  "Get messages (both sent and received) for a persona. Optionally allows to
   only grab messages after a specific message ID."
  (aif (persona-challenge-response-valid-p persona-id challenge-response)
       (alet ((to-persona (get-messages-by-persona persona-id :after after :index "get_messages_to"))
              (from-persona (get-messages-by-persona persona-id :after after :index "get_messages_from"))
              (hash (make-hash-table :test #'equal)))
         (setf (gethash "received" hash) to-persona
               (gethash "sent" hash) from-persona)
         (finish future hash))
       (signal-error future (make-instance 'insufficient-privileges
                                           :msg "Sorry, either the persona you are getting messages for doesn't exist or you don't have access to it."))))

(defafun send-message (future) (message-data challenge)
  "Send a message from one persona to another. The message body is pubkey
   encrypted."
  (let ((from-persona-id (gethash "from" message-data)))
    (add-id message-data)
    (validate-message (message-data future)
      (aif (persona-challenge-response-valid-p from-persona-id challenge)
           (progn
             (alet* ((sock (db-sock))
                     (query (r:r (:insert
                                   (:table "messages")
                                   message-data)))
                     (nil (r:run sock query)))
               (r:disconnect sock)
               (finish future message-data)))
           (signal-error future (make-instance 'insufficient-privileges
                                               :msg "Sorry, either the persona you're sending from doesn't exit, or you don't have access to it."))))))


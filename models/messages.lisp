(in-package :tagit)

(defafun get-messages-for-persona (future) (persona-id challenge-response &key (after ""))
  "Gets messages for a persona. If a message ID is specified for :after, will
   only get messages after that ID."
  (aif (persona-challenge-response-valid-p persona-id challenge-response)
       (alet* ((sock (db-sock))
               (query (r:r (:map
                             (:eq-join
                               (:between
                                 (:table "messages")
                                 :left (list persona-id after)
                                 :right (list persona-id "zzzzzzzzzzzzzzzzzzzzzzzzz")  ;; lol h4x
                                 :index "get_messages")
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
         (finish future res))
       (signal-error future (make-instance 'insufficient-privileges
                                           :msg "Sorry, either the persona you are getting messages for doesn't exist or you don't have access to it."))))

(defafun send-message (future) (from-persona-id from-persona-challenge to-persona-id body)
  "Send a message from one persona to another. The message body is pubkey
   encrypted."
  (aif (persona-challenge-response-valid-p from-persona-id from-persona-challenge)
       (let* ((message (make-hash-table :test #'equal)))
         (add-id message)
         (setf (gethash "from" message) from-persona-id
               (gethash "to" message) to-persona-id
               (gethash "body" message) body)
         (alet* ((sock (db-sock))
                 (query (:insert
                          (:table "messages")
                          message))
                 (nil (r:run sock query)))
           (finish future message)))
       (signal-error future (make-instance 'insufficient-privileges
                                           :msg "Sorry, either the persona you're sending from doesn't exit, or you don't have access to it."))))


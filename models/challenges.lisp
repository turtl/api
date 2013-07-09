(in-package :tagit)

(defvar *challenge-expire* 5
  "How many seconds until a challenge exires.")

(defparameter *challenge-types*
  '(:persona 1)
  "Holds all mappings of keyword <--> integer challenge types.")

(defun get-challenge-type-int (type-keyword)
  "Get the challenge type integer (stored in the DB) from the keyword mapping."
  (getf *challenge-types* type-keyword))

(defun make-challenge (item-id)
  "Generates a challenge based on the items's ID, a crypto-random, and the
   current internal time."
  (sha256 (format nil "~a:~a:~a" (crypto-random) item-id (get-internal-real-time))))

(defun generate-challenge-response (secret challenge)
  "Generates a challenge-response based on given values."
  (sha256 (concatenate 'string secret challenge)))

(defun make-challenge-object (type item-id expire one-time &key add-id)
  "Abstracts making a challenge object passed into the database."
  (let ((hash (make-hash-table :test #'equal))
        (challenge (make-challenge item-id)))
    (setf (gethash "type" hash) (get-challenge-type-int type)
          (gethash "item_id" hash) item-id
          (gethash "challenge" hash) challenge
          (gethash "one_time" hash) (not (not one-time))
          (gethash "expire" hash) (+ expire (get-timestamp)))
    (when add-id (add-id hash))
    hash))
    
(defafun generate-challenge (future) (type item-id &key (expire *challenge-expire*) one-time)
  "Generate a challenge with the given item type/id. Saves the challenge into
   the challenges table for later verification. Challenges will expire after
   *challenge-expire* many seconds, however :expire can be passed to specify a
   different value (in seconds)."
  (alet* ((challenge-obj (make-challenge-object type item-id expire one-time :add-id t))
          (sock (db-sock))
          (query (r:r (:insert
                        (:table "challenges")
                        challenge-obj)))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future (gethash "challenge" challenge-obj) challenge-obj)))

(defafun generate-multiple-challenges (future) (type item-ids &key (expire *challenge-expire*) one-time)
  "Generate multiple challenges for the given type/id list. Follows the same
   method for one-time/expiration as generate-challenge."
  (let ((challenges nil))
    (dolist (item-id item-ids)
      (push (make-challenge-object type item-id expire one-time :add-id t) challenges))
    (alet* ((sock (db-sock))
            (query (r:r (:insert
                          (:table "challenges")
                          (coerce challenges 'vector))))
            (nil (r:run sock query)))
      (r:disconnect sock)
      (let ((res (make-hash-table :test #'equal)))
        (dolist (challenge challenges)
          (setf (gethash (gethash "item_id" challenge) res) (gethash "challenge" challenge)))
        (finish future res)))))

(defafun verify-challenge (future) (type item-id secret response)
  "Verify that a given challenge-response is valid for the given item type/id."
  (alet* ((type-int (get-challenge-type-int type))
          (sock (db-sock))
          (query (r:r (:get-all
                        (:table "challenges")
                        `((,type-int ,item-id))
                        :index "search")))
          (cursor (r:run sock query))
          (challenges (r:to-array sock cursor)))
    (wait-for (r:stop sock cursor)
      (r:disconnect sock))
    (let ((found nil))
      (loop for challenge across challenges do
        (when (<= (get-timestamp) (round (or (gethash "expire" challenge) 0)))
          (let* ((challenge-val (gethash "challenge" challenge))
                 (gen-response (generate-challenge-response secret challenge-val)))
            (when (string= response gen-response)
              (setf found challenge)
              (return)))))
      ;; remove the challenge if it's a one-timer
      (when (and found (gethash "one_time" found))
        (alet* ((challenge-id (gethash "id" found))
                (sock (db-sock))
                (query (r:r (:delete (:get (:table "challenges") challenge-id))))
                (nil (r:run sock query)))
          (r:disconnect sock)))
      (finish future (when found t)))))

;; TODO: finish building
;; TODO: find a way to enumerate failures properly. otherwise, this is more or
;; less good to go (after some testing)
(defafun verify-multiple-challenges (future) (type responses)
  "Verify multiple challenge responses at once. Responses MUST be in the format:
   {
     <item-id>: {
       'secret': 'abc123',   ; this item's secret
       'response': '6969'    ; this item's challenge-response
     },
     ...
   }

   Two values are 'returned' on the future: a list of <item-id>s that were
   successfully verified, and an object of <item-id>s that failed validation in
   the format:
   {
     <item-id>: {
       'code': 1,
       'msg': 'That challenge has expired'
     },
     ...
   }"
  (let* ((type-int (get-challenge-type-int type))
         ;; build a search looking for ALL matching type/item-id pairs in the
         ;; given responses
         (search-clause (loop for item-id being the hash-keys of responses
                              collect `((,type-int ,item-id)))))
    (alet* ((sock (db-sock))
            (query (r:r (:get-all
                          (:table "challenges")
                          search-clause
                          :index "search")))
            (cursor (r:run sock query))
            (challenges (r:to-array sock cursor)))
      (if (r:cursorp cursor)
          (wait-for (r:stop sock cursor)
            (r:disconnect sock))
          (r:disconnect sock))
      (flet ((make-fail (code msg)
               (let ((hash (make-hash-table :test #'equal)))
                 (setf (gethash "code" hash) code
                       (gethash "msg" hash) msg)
                 hash)))
        (let ((success nil)
              (fail (make-hash-table :test #'equal)))
          (loop for challenge across challenges
                for item-id = (gethash "item_id" challenge) do
            ;; don't double-validate successes
            (when (find item-id success) (return))
            (let* ((challenge-val (gethash "challenge" challenge))
                   (response-obj (gethash (gethash "item_id" challenge) responses))
                   (secret (gethash "secret" response-obj))
                   (response (gethash "response" response-obj))
                   (expire (gethash "expire" challenge))
                   (gen-response (generate-challenge-response secret challenge-val)))
              (if (string= gen-response response)
                  (if (<= (get-timestamp) expire)
                      (push item-id success)
                      (setf (gethash item-id fail) (make-fail 2 "Challenge expired.")))
                  (setf (gethash item-id fail) (make-fail 1 "Invalid response.")))
              )))))))


(in-package :tagit)

(defvar *challenge-expire* 5
  "How many seconds until a challenge exires.")

(defparameter *challenge-types*
  '(:persona 1)
  "Holds all mappings of keyword <--> integer challenge types.")

(defun get-type-int (type-keyword)
  "Get the challenge type integer (stored in the DB) from the keyword mapping."
  (getf *challenge-types* type-keyword))

(defun make-challenge (item-id)
  "Generates a challenge based on the items's ID, a crypto-random, and the
   current internal time."
  (sha256 (format nil "~a:~a:~a" (crypto-random) item-id (get-internal-real-time))))

(defun generate-challenge-response (secret challenge)
  "Generates a challenge-response based on given values."
  (sha256 (concatenate 'string secret challenge)))

(defafun generate-challenge (future) (type item-id &key (expire *challenge-expire*) (one-time t))
  "Generate a challenge with the given item type/id. Saves the challenge into
   the challenges table for later verification. Challenges will expire after
   *challenge-expire* many seconds, however :expire can be passed to specify a
   different value (in seconds)."
  (alet* ((challenge (make-challenge item-id))
          (data (let ((hash (make-hash-table :test #'equal)))
                  (add-id hash)
                  (setf (gethash "type" hash) (get-type-int type)
                        (gethash "item_id" hash) item-id
                        (gethash "challenge" hash) challenge
                        (gethash "onetime" hash) (not (not one-time))
                        (gethash "expire" hash) (+ expire (get-timestamp)))
                  hash))
          (sock (db-sock))
          (query (r:r (:insert
                        (:table "challenges")
                        data)))
          (nil (r:run sock query)))
    (r:disconnect sock)
    (finish future challenge data)))

(defafun verify-challenge (future) (type item-id secret response)
  "Verify that a given challenge-response is valid for the given item type/id."
  (alet* ((type-int (get-type-int type))
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
              (setf found challenge)))))
      (when (gethash "onetime" found)
        (alet* ((challenge-id (gethash "id" found))
                (sock (db-sock))
                (query (r:r (:delete (:get (:table "challenges") challenge-id))))
                (nil (r:run sock query)))
          (r:disconnect sock)))
      (finish future (when found t)))))

(cl-rethinkdb-reql::is-array '())


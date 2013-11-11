(in-package :turtl)

(defroute (:get "/api/personas/([0-9a-f-]+)") (req res args)
  "Get a persona by ID."
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (persona (get-persona-by-id persona-id)))
      (if persona
          (send-json res persona)
          (send-json res "Persona not found." :status 404)))))

(defroute (:post "/api/personas") (req res)
  "Add a new persona."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-data (post-var req "data"))
            (persona (add-persona user-id persona-data)))
      (track "persona-add")
      (send-json res persona))))

(defroute (:put "/api/personas/([0-9a-f-]+)") (req res args)
  "Edit a persona."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (car args))
            (persona-data (post-var req "data"))
            (persona (edit-persona user-id persona-id persona-data)))
      (track "persona-edit")
      (send-json res persona))))

(defroute (:delete "/api/personas/([0-9a-f-]+)") (req res args)
  "Delete a persona."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (car args))
            (sync-ids (delete-persona user-id persona-id)))
      (track "persona-delete")
      (let ((hash (make-hash-table :test #'equal)))
        (setf (gethash "sync_ids" hash) sync-ids)
        (send-json res hash)))))

(defroute (:get "/api/personas/email/([a-zA-Z0-9@\/\.\-]+)") (req res args)
  "Get a persona by email (must be an *exact* match as there is no wildcard
   searching or antyhing like that)."
  (catch-errors (res)
    (alet* ((email (car args))
            (ignore-persona-id (get-var req "ignore_persona_id"))
            (require-key-p (= (varint (get-var req "require_key") 0) 1))
            (persona (get-persona-by-email email :ignore-persona ignore-persona-id :require-key require-key-p)))
      (if persona
          (send-json res persona)
          (send-json res "Persona not found ='[" :status 404)))))

;(defroute (:post "/api/personas/([0-9a-f-]+)/challenge") (req res args)
;  (catch-errors (res)
;    (alet* ((persona-id (car args))
;            (expire (min (varint (post-var req "expire") 10) 3600))
;            (persist (if (zerop (varint (post-var req "persist") 0))
;                         nil
;                         t))
;            (challenge (generate-challenge :persona persona-id :expire expire :persist persist)))
;      (send-json res challenge))))

;(defroute (:post "/api/personas/challenges") (req res)
;  (catch-errors (res)
;    (alet* ((persona-ids (yason:parse (post-var req "personas")))
;            (challenges (generate-multiple-challenges :persona persona-ids :expire 1800)))
;      (send-json res challenges))))


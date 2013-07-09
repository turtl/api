(in-package :tagit)

(defroute (:get "/api/personas/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (persona (get-persona-by-id persona-id)))
      (if persona
          (send-json res persona)
          (send-json res "Persona not found." :status 404)))))

(defroute (:post "/api/personas") (req res)
  (catch-errors (res)
    (alet* ((persona-data (post-var req "data"))
            (persona-secret (post-var req "secret"))
            (persona (add-persona persona-secret persona-data)))
      (send-json res persona))))

(defroute (:put "/api/personas/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (challenge-response (post-var req "challenge"))
            (persona-data (post-var req "data"))
            (persona (edit-persona persona-id challenge-response persona-data)))
      (send-json res persona))))

(defroute (:delete "/api/personas/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (challenge-response (post-var req "challenge"))
            (nil (delete-persona persona-id challenge-response)))
      (send-json res t))))

(defroute (:get "/api/personas/screenname/([a-zA-Z0-9\/\.]+)") (req res args)
  (catch-errors (res)
    (alet* ((screenname (car args))
            (ignore-persona-id (get-var req "ignore_persona_id"))
            (persona (get-persona-by-screenname screenname ignore-persona-id)))
      (if persona
          (send-json res persona)
          (send-json res "Persona not found ='[" :status 404)))))

(defroute (:post "/api/personas/([0-9a-f-]+)/challenge") (req res args)
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (challenge (generate-challenge :persona persona-id :expire 5 :one-time t)))
      (send-json res challenge))))

(defroute (:post "/api/personas/challenges") (req res)
  (catch-errors (res)
    (alet* ((persona-ids (ignore-errors (yason:parse (post-var req "personas"))))
            (challenges (generate-multiple-challenges :persona persona-ids :expire 1800)))
      (send-json res challenges))))


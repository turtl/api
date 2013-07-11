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

(defroute (:get "/api/personas/screenname/([a-zA-Z0-9\/\.]+)\\*") (req res args)
  (catch-errors (res)
    (alet* ((screenname (car args))
            (personas (search-personas :screenname screenname)))
      (send-json res personas))))

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
            (expire (min (varint (post-var req "expire") 10) 3600))
            (persist (if (zerop (varint (post-var req "persist") 0))
                         nil
                         t))
            (challenge (generate-challenge :persona persona-id :expire expire :persist persist)))
      (send-json res challenge))))

(defroute (:post "/api/personas/challenges") (req res)
  (catch-errors (res)
    (alet* ((persona-ids (yason:parse (post-var req "personas")))
            (challenges (generate-multiple-challenges :persona persona-ids :expire 1800)))
      (send-json res challenges))))


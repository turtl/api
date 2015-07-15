(in-package :turtl)

(defroute (:get "/api/personas/([0-9a-f-]+)") (req res args)
  "Get a persona by ID."
  (catch-errors (res)
    (alet* ((persona-id (car args))
            (persona (get-persona-by-id persona-id)))
      (if persona
          (send-json res persona)
          (send-json res "Persona not found." :status 404)))))

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


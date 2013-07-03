(in-package :tagit)

(defroute (:get "/api/personas/screenname/([a-zA-Z0-9\/\.]+)") (req res args)
  (catch-errors (res)
    (alet* ((screenname (car args))
            (persona (get-persona-by-screenname screenname)))
      (if persona
          (send-json res persona)
          (send-json res "Persona not found ='[" :status 404)))))


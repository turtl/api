(in-package :tagit)

(defvalidator validate-user
  (("id" :type string :required t :length 24)
   ("screenname" :type string :required t)
   ("email" :type string)
   ("name" :type string)))

(defafun get-persona-by-screenname (future) (screenname)
  "Grab a persona via its screenname. Must be an exact match (for now)."
  (alet* ((sock (db-sock))
          (query (r:r (:limit
                        (:get-all (:table "personas")
                                  screenname
                                  "screenname")
                        1)))
          (cursor (r:run sock query))
          (persona (when (r:has-next cursor)
                     (r:next sock cursor))))
    (r:stop sock cursor)
    (r:disconnect sock)
    (finish future persona)))


(in-package :turtl)

(defafun get-promo-by-code (future) (code)
  "Get a promo by its code."
  (alet* ((sock (db-sock))
          (query (r:r (:limit
                        (:get-all
                          (:table "promo")
                          code
                          :index (db-index "promo" "code"))
                        1)))
          (cursor (r:run sock query))
          (promos (r:to-array sock cursor))
          (promo (when (< 0 (length promos))
                   (aref promos 0))))
    (r:stop/disconnect sock cursor)
    (finish future promo)))

(defun apply-promo (user promo &key count-uses)
  "Apply a promo to a user object. Note that we don't actually save anything to
   the user's account...this function is mean to be run in-place *before* a user
   is added ot the DB.
   
   This destructively modifies the user object.
   
   If count-uses is specified, this function increments the `uses` field in the
   promo and returns a second value (a future) that finishes when the update is
   done."
  ;; make sure the promo isn't expired
  (when (promo-expired-p promo)
    (return-from apply-promo))
  (let* ((type :storage))
    (case type
      (:storage
        (let ((user-storage (gethash "storage" user))
              (promo-storage (gethash "storage" promo)))
          (when (and user-storage
                     (< promo-storage user-storage))
            ;; nothing to do, user already has more storage than promo gives
            (return-from apply-promo))
          (setf (gethash "storage" user) (round promo-storage))))))
  (if count-uses
      ;; do our own error handline here since this isn't your normal async
      ;; function
      (catcher
        (alet* ((sock (db-sock))
                (query (r:r (:update
                              (:get (:table "promo") (gethash "id" promo))
                              (r:fn (p)
                                `(("uses" . ,(:+ (:attr p "uses") 1)))))))
                (nil (r:run sock query)))
          (1+ (gethash "uses" promo)))
        (error (e)
          ;; just log it
          (vom:error "apply-promo: count uses: ~a" e)
          (add-server-log e (format nil "apply-promo/count-uses"))))
      user))

(defun promo-expired-p (promo)
  "Check if a promo is expired."
  (let* ((max-uses (gethash "max_uses" promo))
         (uses (gethash "uses" promo))
         (expires (gethash "expires" promo))
         (expired nil))
    (when (and max-uses
               (< 0 max-uses)
               (<= max-uses uses))
      (setf expired t))
    (when (and expires
               (< 0 expires)
               (<= expires (get-timestamp)))
      (setf expired t))
    expired))


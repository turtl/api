(in-package :turtl)

(defafun get-board-size (future) (board-id)
  "Grab the size of a board, in bytes (notes: note body + note file size)."
  (alet* ((sock (db-sock))
          (query (r:r
                   (:reduce
                     (:map
                       (:get-all
                         (:table "notes")
                         board-id
                         :index (db-index "notes" "boards"))
                       (:js "(function(note) { return ((note.body && note.body.length) || 0) + ((note.file && note.file.size) || 0); })"))
                     (r:fn (a b) (:+ a b))
                     :base 0)))
          (size (r:run sock query)))
    (r:disconnect sock)
    (finish future (round size))))

(defafun get-profile-size (future) (user-id)
  "Grab a user's profile size (only the boards they own)."
  (alet* ((board-ids (get-all-user-board-ids user-id :shared nil))
          (board-ids (coerce board-ids 'list))
          (size 0))
    (wait (adolist (board-id board-ids)
            (alet ((board-size (get-board-size board-id)))
              (incf size board-size)))
      (finish future size))))


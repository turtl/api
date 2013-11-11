(in-package :turtl)

;;; Many of the routes in this controller look for a persona ID passed with the
;;; request, and if found use that insteda of the currently logged-in user when
;;; updating note data. The purpose of this is to use a persona's permissions to
;;; validate changing note data intstead of the user's (in the case that the
;;; note is in a board the persona has share access to).
;;;
;;; If a persona ID is apssed, it is *always* used. The client must make the
;;; decision about whether or not to pass it, the server is not going to take
;;; both and use the one with the highest permissions.

;; TODO: just POST /notes instead of including the board id...
(defroute (:post "/api/boards/([0-9a-f-]+)/notes") (req res args)
  "Add a note. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (post-var req "persona"))
            (board-id (car args))
            (note-data (post-var req "data"))
            (note (if persona-id
                      (with-valid-persona (persona-id user-id)
                        (add-note user-id board-id note-data :persona-id persona-id))
                      (add-note user-id board-id note-data))))
      (track "note-add" `(:shared ,(when persona-id t)))
      (send-json res note))))

(defroute (:put "/api/notes/([0-9a-f-]+)") (req res args)
  "Edit a note. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (note-data (post-var req "data"))
            (note (if persona-id
                      (with-valid-persona (persona-id user-id)
                        (edit-note persona-id note-id note-data))
                      (edit-note user-id note-id note-data))))
      (track "note-edit" `(:shared ,(when persona-id t)))
      (send-json res note))))

(defroute (:delete "/api/notes/([0-9a-f-]+)") (req res args)
  "Delete a note. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (sync-ids (if persona-id
                          (with-valid-persona (persona-id user-id)
                            (delete-note persona-id note-id))
                          (delete-note user-id note-id))))
      (track "note-delete" `(:shared ,(when persona-id t)))
      (let ((hash (make-hash-table :test #'equal)))
        (setf (gethash "sync_ids" hash) sync-ids)
        (send-json res hash)))))

(defroute (:put "/api/notes/batch") (req res)
  "Batch edit. Allows passing in a persona ID, which will be used in place of
   the current user ID when validating permissions."
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (post-var req "persona"))
            (batch-edit-data (post-var req "data"))
            (nil (if persona-id
                     (with-valid-persona (persona-id user-id)
                       (batch-note-edit persona-id batch-edit-data))
                     (batch-note-edit user-id batch-edit-data))))
      (send-json res t))))


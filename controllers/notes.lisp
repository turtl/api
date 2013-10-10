(in-package :turtl)

(defroute (:post "/api/boards/([0-9a-f-]+)/notes") (req res args)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (post-var req "persona"))
            (board-id (car args))
            (note-data (post-var req "data"))
            (has-file (= (varint (post-var req "file") 0) 1))
            (file (when has-file (make-file)))
            (nil (when file
                   (setf (gethash "file_id" note-data) (gethash "id" file))
                   (add-file user-id file)))
            (note (if persona-id
                      (with-valid-persona (persona-id user-id)
                        (add-note user-id board-id note-data :persona-id persona-id))
                      (add-note user-id board-id note-data))))
      (when file
        (setf (gethash "file_id" note) (gethash "id" file)))
      (track "note-add" `(:shared ,(when persona-id t) :file ,has-file))
      (send-json res note))))

(defroute (:put "/api/notes/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (note-data (post-var req "data"))
            (note (if persona-id
                      (with-valid-persona (persona-id user-id)
                        (edit-note persona-id note-id note-data))
                      (edit-note user-id note-id note-data)))
            (has-file (and (not (gethash "file_id" note))
                           (= (varint (post-var req "file") 0) 1)))
            (file (when has-file (make-file)))
            (note (if file
                      (let ((note-with-file (make-hash-table :test #'equal)))
                        (setf (gethash "id" note-with-file) (gethash "id" note)
                              (gethash "file_id" note-with-file) (gethash "id" file))
                        (add-file user-id file)
                        ;; re-save note with new file-id
                        (if persona-id
                            (with-valid-persona (persona-id user-id)
                              (edit-note persona-id note-id note-with-file))
                            (edit-note user-id note-id note-with-file)))
                      note)))
      (track "note-edit" `(:shared ,(when persona-id t)))
      (send-json res note))))

(defroute (:delete "/api/notes/([0-9a-f-]+)") (req res args)
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (nil (if persona-id
                    (with-valid-persona (persona-id user-id)
                      (delete-note persona-id note-id))
                    (delete-note user-id note-id))))
      (track "note-delete" `(:shared ,(when persona-id t)))
      (send-json res t))))

(defroute (:delete "/api/notes/([0-9a-f-]+)/file") (req res args)
  (catch-errors (res)
    (alet* ((note-id (car args))
            (user-id (user-id req))
            (persona-id (post-var req "persona"))
            (nil (if persona-id
                     (with-valid-persona (persona-id user-id)
                       (delete-note-file persona-id note-id))
                     (delete-note-file user-id note-id))))
      (track "file-delete" `(:shared ,(when persona-id t)))
      (send-json res t))))

(defroute (:put "/api/notes/batch") (req res)
  (catch-errors (res)
    (alet* ((user-id (user-id req))
            (persona-id (post-var req "persona"))
            (batch-edit-data (post-var req "data"))
            (nil (if persona-id
                     (with-valid-persona (persona-id user-id)
                       (batch-note-edit persona-id batch-edit-data))
                     (batch-note-edit user-id batch-edit-data))))
      (send-json res t))))


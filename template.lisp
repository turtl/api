(in-package :tagit)

(define-condition view-not-found (error)
  ((view :initarg :view :reader view-name :initform nil))
  (:report (lambda (c s) (format s "View not found: ~a" (view-name c))))
  (:documentation "Describes the condition when a view doesn't exist."))

(defvar *views* nil
  "Defines a container for all parsed/compiled views to be cached.")

(defmacro deflayout (name (data-var &key (stream-var 's) top-level) &body body)
  "Define a layout function. Can be used in conjunction with a 'layout: ...'
   header in a markdown file."
  (let ((view-name (intern (string-upcase (format nil "layout-~a" name)) :tagit)))
    `(progn
       (defun ,view-name (,data-var)
         (cl-who:with-html-output-to-string (,stream-var nil :prologue ,top-level :indent t)
           ,@body))
       ',view-name)))

(defmacro parent-layout (name data-var &body body)
  "Allows a layout to be wrapped by another (parent) layout."
  `(str
     (layout ,name (append (list :content (cl-who:with-html-output-to-string (s)
                                            ,@body))
                           ,data-var))))

(defmacro defview (view-name (data-var &key (stream-var 's)) &body body)
  "Define a view."
  (let ((view-name (intern (symbol-name view-name) :keyword)))
    `(setf (gethash ,view-name *views*)
           (list :fn (lambda (,data-var)
                       (cl-who:with-html-output-to-string (,stream-var nil :prologue nil :indent t)
                         ,@body))))))

(defun generate-view-name (view-dir file)
  "Generates a symbol that can be used to identify a view."
  (let* ((view-dir (namestring view-dir))
         (file (namestring file))
         (file (subseq file (1+ (mismatch file view-dir))))
         (file (subseq file 0 (position #\. file :from-end t))))
    (intern (string-upcase file) :keyword)))

(defun load-views (&key subdir (clear t) (view-directory (format nil "~a/views" *root*)))
  "Load and cache all view files."
  (when clear
    (setf *layouts* nil
          *views* (make-hash-table :test 'eq)))
  (dolist (file (cl-fad:list-directory (if subdir subdir view-directory)))
    (cond ((cl-fad:directory-exists-p file)
           (load-views :subdir file :clear nil))
          ((cl-fad:file-exists-p file)
           (let* ((file-str (namestring file))
                  (ext (subseq file-str (or (position #\. file-str :from-end t) (length file-str))))
                  (view-name (generate-view-name view-directory file-str)))
             (cond ((string= ext ".lisp")
                    (load file)))))))
  *views*)

(defun layout (name data)
  "Sends data to a layout and returns the content for that layout."
  (let ((layout-fn (intern (string-upcase (format nil "layout-~a" name)) :tagit)))
    (funcall layout-fn data)))

(defun load-view (name &key data headers)
  "Load a view."
  (let ((view (gethash name *views*)))
    (if view
        (let* ((fn (getf view :fn))
               (html (getf view :html))
               (meta (append headers (getf view :meta)))
               (html (if fn
                         (funcall fn data)
                         html))
               (layout (getf meta :layout))
               (html (if layout
                         (layout layout (list :title (getf meta :title)
                                              :content html))
                         html)))
          html)
        (error 'view-not-found :view name))))


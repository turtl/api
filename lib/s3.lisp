(in-package :turtl)

(defparameter +nl+ (format nil "~c" #\newline)
  "Should use defconstant, but NOOoOoO it doesn't support strings")
  
(defun combine-headers (headers)
  "Combine same-name headers into one value."
  (let ((new-headers nil)
        (last-header-name ""))
    (dolist (header headers)
      (let ((name (car header))
            (value (string-trim #(#\space #\tab #\newline #\return) (cdr header))))
        (if (string= name last-header-name)
            (setf (cdr (car new-headers)) (concatenate 'string
                                                       (cdr (car new-headers))
                                                       ","
                                                       value))
            (push (cons name value) new-headers))
        (setf last-header-name name)))
    (nreverse new-headers)))

(defun make-s3-sig (http-verb resource &key (content-md5 "") (content-type "") (date "") amz-headers)
  "Makes an S3 auth signature give the values provided. `headers` must be an
   alist."
  (let* ((headers (sort amz-headers (lambda (a b)
                                      (string<
                                        (string-downcase (car a))
                                        (string-downcase (car b))))))
         (headers (combine-headers headers))
         (headers (reduce
                    (lambda (a b)
                      (let* ((name (car b))
                             (name (string-downcase name))
                             (name (string-trim #(#\space #\tab #\newline #\return) name))
                             (value (cdr b))
                             (value (string-trim #(#\space #\tab #\newline #\return) value)))
                        (concatenate 'string
                                     a
                                     name ":" value
                                     +nl+)))
                    headers
                    :initial-value ""))
         (headers (string-trim #(#\space #\tab #\newline #\return) headers)))
    (concatenate 'string
                 http-verb +nl+
                 content-md5 +nl+
                 content-type +nl+
                 date +nl+
                 (unless (string= headers "") (concatenate 'string headers +nl+))
                 resource)))

(defun hash-s3-sig (key sig-str &key (base64 t))
  "HMAC-SHA1 a signature str with the given key."
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key) 'ironclad:sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array sig-str))
    (let ((hash (ironclad:hmac-digest hmac)))
      (if base64
          (cl-base64:usb8-array-to-base64-string hash)
          hash))))

(defun s3-op (method resource &key content (content-md5 "") (content-type "") headers (config *amazon-s3*) read-timeout write-timeout)
  (let* ((date (local-time:format-timestring
                 nil
                 (local-time:now)
                 :format local-time:+rfc-1123-format+))
         (resource (concatenate 'string "/" (getf config :bucket) resource))
         (amz-headers (remove-if-not
                        (lambda (header)
                          (string= (string-downcase (car header))
                                   "x-amz-"))
                        headers))
         (sig (make-s3-sig
                (string method)
                resource
                :content-md5 content-md5
                :content-type content-type
                :date date
                :amz-headers amz-headers))
         (signature (hash-s3-sig (getf config :secret) sig))
         (auth-header (concatenate 'string
                                   "AWS "
                                   (getf config :token) ":"
                                   signature))
         (headers (append
                    headers
                    `(("Authorization" . ,auth-header)
                      ("Date" . ,date))
                    (unless (string= content-md5 "")
                      `(("Content-MD5" . ,content-md5)))))
         (url (concatenate 'string
                           (getf config :endpoint)
                           resource)))
    (drakma-async:http-request
      url
      :method method
      :content content
      :content-type (unless (string= content-type "") content-type)
      :content-length (when (vectorp content) (length content))
      :read-timeout read-timeout
      :write-timeout write-timeout
      :additional-headers headers)))

(defun get-upload-id (xml)
  "Given an S# multipart upload start response, get the UploadID."
  (let* ((xml (cl-ppcre:regex-replace "<\\?.*?\\?>" xml ""))
         (tree (xmls:parse xml)))
    (dolist (child (cdr tree))
      (when child
        (let* ((tag (car child))
               (tag (if (listp tag)
                        (car tag)
                        tag))
               (value (cadr (cdr child))))
          (when (string= (string-downcase tag) "uploadid")
            (return-from get-upload-id value)))))))

(define-condition s3-upload-error (error)
  ((msg :initarg :msg :reader msg :initform nil)
   (upload-id :initarg :upload-id :reader upload-id :initform nil))
  (:report (lambda (c s) (format s "S3 upload error: ~a: ~a" (upload-id c) (msg c))))
  (:documentation "Describes an S3 upload error."))

(defun build-upload-completion (parts)
  "Takes an alist of (part-num . etag) and builds a bunch of shitty, annoying
   XML out of it."
  (let ((str "<CompleteMultipartUpload>")
        (parts (sort parts (lambda (a b) (< (car a) (car b))))))
    (dolist (part parts)
      (setf str (concatenate 'string
                             str
                             "<Part>"
                             "<PartNumber>" (write-to-string (car part)) "</PartNumber>"
                             "<ETag>" (cdr part) "</ETag>"
                             "</Part>")))
    (concatenate 'string str "</CompleteMultipartUpload>")))

(defun get-s3-error (xml)
  "If an error exists in the provided XML dump, parse out the message, otherwise
   return nil (no error)."
  (let* ((xml (if (stringp xml)
                  xml
                  (babel:octets-to-string xml)))
         (xml (cl-ppcre:regex-replace "<\\?.*?\\?>" xml ""))
         (tree (xmls:parse xml))
         (code nil)
         (msg nil))
    (dolist (tag (cddr tree))
      (let* ((tagname (car tag))
             (tagname (if (stringp tagname)
                          tagname
                          (car tagname)))
             (value (caddr tag)))
        (cond ((string= tagname "Code")
               (setf code value))
              ((string= tagname "Message")
               (setf msg value)))
        (when (and code msg)
          (return))))
    (when (and code msg)
      (format nil "~a: ~a" code msg))))

(defun s3-upload-abort (resource upload-id)
  "Cancel an S3 upload."
  (let ((future (make-future)))
    (multiple-future-bind (res status)
        (s3-op :delete (concatenate 'string resource "?uploadId=" upload-id))
      (let* ((res (if (stringp res)
                      res
                      (babel:octets-to-string res))))
        (if (<= 200 status 299)
            (finish future t)
            (signal-error future (make-instance 's3-upload-error :upload-id upload-id :msg (get-s3-error res))))))
    future))

(defun s3-upload (resource &key (content-type "application/octet-stream") headers)
  "Makes an uploader that allows sending data to S3 in chunks. Returns a future
   that finishes with a function. The function is the same API as drakma's
   `:content :continuation` lambda: two args the first is `data` and the second
   is `continuep` (T if more data on the way, nil if finished).
   
   Once the last chunk is sent (`continuep` == nil) it automatically finishes
   the upload with S3."
  ;; make a future that will be finished with our continuation function (or an
  ;; error if things go awry)
  (let ((future (make-future)))
    (log:debug "s3: creating upload")
    (future-handler-case
      ;; make our initial request, humbly groveling at S3's feet for the
      ;; privilege of using their proprietary multi-part upload system when HTTP
      ;; already has a great chunking protocol built-in that everybody has used
      ;; for years.
      (multiple-future-bind (res status)
          (s3-op :post
                 (concatenate 'string resource "?uploads")
                 :content-type content-type
                 :content ""
                 :headers headers)
        ;; see if S3 was kind enough to issue us an upload id
        (let* ((res (if (stringp res)
                        res
                        (babel:octets-to-string res)))
               (upload-id (get-upload-id res))
               (min-part-size (* 5 1024 1024))   ; 5MB LOL
               (last-chunk-sent nil))
          (if (and res (<= 200 status 299) upload-id)
              ;; wow, THANK YOU S3, your wisdom is exceeded only by your
              ;; kindness. let's finish our future with a function that when
              ;; called will upload our parts for us
              (future-handler-case
                (let ((part-num 1)
                      (part (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
                      (finished-parts nil))
                  (log:debug "s3: upload started: ~a" upload-id)
                  (log:debug "s3: returning continuation lambda")
                  (finish future
                    (lambda (data &optional continuep)
                      ;; append the data we got onto our current part. we keep
                      ;; doing this until either we have the last chunk or the
                      ;; part length is greater than the min part size (5MB in our
                      ;; case)
                      ;(log:debug "s3: appending data to part: ~a" (stream-length part))
                      (setf last-chunk-sent (or (not continuep) last-chunk-sent))
                      (write-sequence data part)
                      (when (or (not continuep)
                                (<= min-part-size (stream-length part)))
                        (log:debug "s3: chunk overflow size hit (or last chunk): ~a" (stream-length part))
                        ;; ok, we're either done uploading or at the 5MB mark. run
                        ;; the upload of this part
                        (let* ((future (make-future))
                               (local-part-num part-num)
                               (part-data (flexi-streams:get-output-stream-sequence part))
                               (part-md5 (md5 part-data :base64 t))
                               (resource-part
                                 (concatenate 'string
                                              resource
                                              "?partNumber=" (format nil "~a" local-part-num)
                                              "&uploadId=" upload-id)))
                          ;; abort the upload on any error
                          (attach-errback
                            future
                            (lambda (&rest _)
                              (declare (ignore _))
                              (log:warn "s3: aborting upload ~a" upload-id)
                              (s3-upload-abort resource upload-id)))
                          (incf part-num)
                          ;; send the actual request, using our handy s3-op 
                          ;; unction
                          (log:debug "s3: sending part ~a to S3: ~a" local-part-num (length part-data))
                          (multiple-future-bind (res status headers)
                              (s3-op :put resource-part
                                     :content part-data
                                     :content-md5 part-md5
                                     :read-timeout nil
                                     :write-timeout 30)
                            (setf part-data nil)
                            ;; OH NO!! now we have to parse XML to pull out an
                            ;; error string!! where did my life go so wrong?
                            (log:debug "s3: part ~a ret: ~a" local-part-num status)
                            (unless (<= 200 status 299)
                              (error 's3-upload-error :upload-id upload-id :msg (get-s3-error res)))
                            ;; make sure we save our etags! god forbid we don't
                            ;; keep meticulous track of the data we're already
                            ;; uploading using S3's proprietary chunking API
                            (let ((etag (cdr (assoc :etag headers))))
                              (log:debug "s3: saving part ~a etag: ~a" local-part-num etag)
                              (push (cons local-part-num etag) finished-parts))
                            (log:debug "s3: num finished/parts (~a): ~a ~a" last-chunk-sent (length finished-parts) (1- part-num))
                            (if (or (not last-chunk-sent)
                                    (< (length finished-parts) (1- part-num)))
                                ;; T is "ALL DONE" nil is "still have chunks to process"
                                (finish future nil)
                                ;; looks like we're done. finish the upload
                                (let* ((body (build-upload-completion finished-parts))
                                       (resource-final
                                         (concatenate 'string
                                                      resource
                                                      "?uploadId=" upload-id)))
                                  (log:debug "s3: upload chunking done, finalize upload")
                                  (multiple-future-bind (res)
                                      (s3-op :post resource-final
                                             :content body)
                                    ;; need to test for errors manually since S3
                                    ;; breaks HTTP yet again by passing 200 OK
                                    ;; even if there's a problem
                                    (let ((err-str (get-s3-error res)))
                                      (log:debug "s3: finalize response err (nil is good): ~a" err-str)
                                      (if err-str
                                          (signal-error future (make-instance 's3-upload-error :upload-id upload-id :msg err-str))
                                          (finish future t)))))))
                          future)))
                    upload-id))
                (t (e)
                  (log:warn "s3: aborting upload ~a" upload-id)
                  (s3-upload-abort resource upload-id)
                  (signal-error future e)))
              (signal-error future (make-instance 's3-upload-error :msg "Error starting upload.")))))
      (t (e) (signal-error future e)))
    future))


(defconstant +nl+ (format nil "~c" #\newline))
  
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
  "Makes an S3 auth signature give the values provided. `amz-headers` must be an
   alist."
  (let* ((headers (sort amz-headers (lambda (a b) (string< (car a) (car b)))))
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
                 headers +nl+
                 resource)))

(defun hash-s3-sig (key sig-str &key (base64 t))
  "HMAC-SHA1 a signature str with the given key."
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key) 'ironclad:sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array sig-str))
    (let ((hash (ironclad:hmac-digest hmac)))
      (if base64
          (cl-base64:usb8-array-to-base64-string hash)
          hash))))

(defun s3-request
(let* ((sig (make-s3-sig
              "PUT"
              "/quotes/nelson"
              :content-md5 "c8fdb181845a4ca6b8fec737b3581d76"
              :content-type "text/html"
              :date "Thu, 17 Nov 2005 18:49:58 GMT"
              :amz-headers '(("X-Amz-Meta-Author" . "foo@bar.com")
                             ("X-Amz-Magic" . "abracadabra"))))
       (hash (hash-s3-sig "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV" sig)))
  hash)
  
  

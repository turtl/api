(in-package :turtl)

(defun sha256 (sequence/string)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (string-downcase
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence (ironclad:make-digest 'ironclad:sha256)
                                (if (stringp sequence/string)
                                    (babel:string-to-octets sequence/string)
                                    sequence/string)))))

(defun hmac (string password)
  "Quick HMAC function."
  (let* ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array password) :sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array string))
    (ironclad:hmac-digest hmac)))

(defun md5 (sequence/string &key base64 raw)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (declare (optimize (speed 3)))
  (let* ((sequence (if (stringp sequence/string)
                       (babel:string-to-octets sequence/string)
                       sequence/string))
         ;(sequence (coerce sequence '(vector (unsigned-byte 8))))
         (sequence (coerce sequence '(simple-array (unsigned-byte 8))))
         ;(hash (ironclad:digest-sequence :md5 sequence))
         (hash (md5:md5sum-sequence sequence)))
    (if base64
        (base64:usb8-array-to-base64-string hash)
        (if raw
            hash
            (string-downcase (ironclad:byte-array-to-hex-string hash))))))

(defun read-binary-file (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let* ((len (file-length s))
           (data (make-array len :element-type '(unsigned-byte 8)))
           (num-bytes (read-sequence data s)))
      (values data num-bytes))))

(defun md5-test (&optional (path "/htdocs/tmp/fool.mp3"))
  (let* ((bytes (read-binary-file path))
         (hash (md5 bytes :base64 nil)))
    hash))

(defun crypto-random ()
  "Generate a cryptographically-secure random number between 0 and 1. Works by
   calling out to OpenSSL."
  (coerce (/ (secure-random:number most-positive-fixnum) most-positive-fixnum) 'single-float))



(in-package :turtl)

(defun sha256 (sequence/string)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (string-downcase
    (to-hex
      (ironclad:digest-sequence (ironclad:make-digest 'ironclad:sha256)
                                (if (stringp sequence/string)
                                    (babel:string-to-octets sequence/string)
                                    sequence/string)))))

(defun md5 (sequence/string &key base64 raw)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (let ((hash (ironclad:digest-sequence
                (ironclad:make-digest 'ironclad:md5)
                (coerce 
                  (if (stringp sequence/string)
                      (babel:string-to-octets sequence/string)
                      sequence/string)
                  '(simple-array (unsigned-byte 8))))))
    (if base64
        (base64:usb8-array-to-base64-string hash)
        (if raw
            hash
            (string-downcase (to-hex hash))))))

(defun crypto-random ()
  "Generate a cryptographically-secure random number between 0 and 1. Works by
   calling out to OpenSSL."
  (coerce (/ (secure-random:number most-positive-fixnum) most-positive-fixnum) 'single-float))



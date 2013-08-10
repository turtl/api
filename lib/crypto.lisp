(in-package :tagit)

(defun sha256 (sequence/string)
  "Return a *string* sha256 hash of the given string/byte sequence."
  (string-downcase
    (to-hex
      (ironclad:digest-sequence (ironclad:make-digest 'ironclad:sha256)
                                (if (stringp sequence/string)
                                    (babel:string-to-octets sequence/string)
                                    sequence/string)))))

(defun crypto-random ()
  "Generate a cryptographically-secure random number between 0 and 1. Works by
   calling out to OpenSSL."
  (coerce (/ (secure-random:number most-positive-fixnum) most-positive-fixnum) 'single-float))



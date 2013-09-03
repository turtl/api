(in-package :turtl)

(defafun track (future) (event &optional data)
  "Tracks an analytics event."
  (if (getf *mixpanel* :enabled)
      (let ((properties (convert-plist-hash data)))
        (setf (gethash "token" properties) (getf *mixpanel* :token))
        (alet* ((request (convert-plist-hash
                           `(:event ,event
                             :properties ,properties)))
                (request-json (with-output-to-string (s)
                                (yason:encode request s)))
                (request-base64 (base64:string-to-base64-string request-json))
                (res (drakma-async:http-request
                       "https://api.mixpanel.com/track"
                       :method :post
                       :parameters `(("data" . ,request-base64))))
                (res (babel:octets-to-string res)))
          (if (string= res "1")
              (finish future t)
              (finish future nil))))
      (finish future t)))


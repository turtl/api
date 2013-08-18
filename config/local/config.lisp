(in-package :tagit)

(defparameter *root* (asdf:system-relative-pathname :tagit #P"")
  "Defines the root directory tagit is loading from (basically the ASDF path).")

(defvar *db-name* "tagit"
  "The name of the database we'll be using LOL")

(defvar *db-host* "127.0.0.1"
  "The database hostname/ip.")

(defvar *db-port* 28015
  "The database port number.")

(defvar *site-url* "http://tagit.dev:81"
  "The main URL the site will load from.")

(defvar *site-assets* (namestring (truename (format nil "~a../js" *root*)))
  "Where the static files are.")

(defvar *api-url* "/api"
  "The location (absolute or relative to *site-url*) that API calls will go to.")

(defvar *api-key* "12345"
  "The API key used for the app.")

(defvar *admin-email* "andrew@lyonbros.com"
  "The email used for admin communications. This is reported to users on a
   server error, and possibly other instances.")

(defvar *email-from* "noreply@tagit.beeets.com"
  "The email address all tagit emails come from.")

(defvar *email-user* ""
  "The username used for sending email. Needs to be set on load.")
(defvar *email-pass* ""
  "The password used for sending email. Needs to be set on load.")

(defvar *display-errors* t
  "Whether or not to show errors in HTTP responses. Useful for debugging, bad
   for production.")

(defparameter *public-actions*
  `((:post . "/api/users")
    (:get . ,(cl-ppcre:create-scanner "/api/personas/screenname/([a-zA-Z0-9\/\.]+)"))
    (:get . ,(cl-ppcre:create-scanner "/api/invites/codes/([0-9a-f-]+)")))
  "A list of public resources/actions that do not require authentication.")

;; setup the wookie log
(setf *log-level* :notice)


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

(defvar *api-url* "/api"
  "The location (absolute or relative to *site-url*) that API calls will go to.")

(defvar *api-key* "12345"
  "The API key used for the app.")

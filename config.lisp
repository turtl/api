(in-package :tagit)

(defparameter *root* (asdf:system-relative-pathname :tagit #P"")
  "Defines the root directory tagit is loading from (basically the ASDF path).")

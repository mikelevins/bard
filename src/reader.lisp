(in-package :bard.internal)

(defparameter $last-err nil)

(defun read (&optional input-stream eof-error-p eof-value recursive-p)
  (cl:read input-stream eof-error-p eof-value recursive-p))

(defmethod read-from-string ((s string))
  (with-input-from-string (in s)
    (read in)))

;;; (read-from-string "2")
;;; (read-from-string "nothing")
;;; (read-from-string ":bar")
;;; (read-from-string "foo:bar")

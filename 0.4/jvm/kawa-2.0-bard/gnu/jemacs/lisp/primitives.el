(defun integerp (object)
  (typep object 'integer))

(defun listp (object)
  (typep object 'list))

(defun characterp (object)
  (typep object 'character))

(defun bufferp (object)
  (typep object 'buffer))

(defun markerp (object)
  (typep object 'marker))

(defun zerop (x)
  (= x 0))

(defun define-function (symbol object) (fset symbol object))

(defvar emacs-version
  (format "0%s JEmacs" (substring (scheme-implementation-version) 1)))

(defvar help-char ?\C-g)

(defun add-hook (hook value &optional append local) nil)  ;; Ignore for now.  FIXME.

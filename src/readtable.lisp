(in-package :bard.internal)
;;; (in-readtable :STANDARD)
;;; using in-readtable in this file causes { and [ to fail to read correctly
;;; even if we set the readtable to :common-lisp or :standard

(defreadtable :bard
  (:merge :standard)
  (:syntax-from :standard #\) #\])
  (:syntax-from :standard #\) #\})
  (:macro-char #\[ #'(lambda (stream char)(cons '|list| (read-delimited-list #\] stream))))
  (:macro-char #\{ #'(lambda (stream char) (cons '|map| (read-delimited-list #\} stream))))
  (:macro-char #\# :dispatch)
  (:case :preserve))

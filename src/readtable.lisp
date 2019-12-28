(in-package :bard.internal)
;;; (in-readtable :STANDARD)
;;; using in-readtable in this file causes { and [ to fail to read correctly
;;; even if we set the readtable to :common-lisp or :standard

(defreadtable :bard
  (:merge :standard)
  (:syntax-from :standard #\) #\])
  (:syntax-from :standard #\) #\})
  (:macro-char #\[ #'(lambda (stream char)(read-delimited-list #\] stream)))
  (:macro-char #\{ #'(lambda (stream char)
                       (fset:convert 'fset:map
                                     (loop for tail on (read-delimited-list #\} stream) by #'cddr
                                        collect (cons (first tail)(second tail))))))
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\t t)
  (:dispatch-macro-char #\# #\f nil)
  (:case :preserve))

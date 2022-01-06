;;;; bard.lisp
;;;; literal syntax for data structures

(IN-PACKAGE :bard.internal)

;;; vectors
;;; ---------------------------------------------------------------------
;;; enables us to write literal vectors like [1 2 3 [4 5] 6]

(SET-MACRO-CHARACTER #\[
                     (^ (stream char)
                       (DECLARE (IGNORE char))
                       (LET ((elts (READ-DELIMITED-LIST #\] stream T)))
                         ` (CL:VECTOR ,@elts))))

(SET-MACRO-CHARACTER #\] (GET-MACRO-CHARACTER #\)))

;;; dicts
;;; ---------------------------------------------------------------------
;;; enables us to write literal dicts like {:a 1 :b 2 :c {:d 4 :e 5}}
;;; dicts created this way always use 'equal to test keys

(SET-SYNTAX-FROM-CHAR #\{ #\()
(SET-SYNTAX-FROM-CHAR #\} #\))

(SET-MACRO-CHARACTER #\{
                     (^ (stream char)
                       (DECLARE (IGNORE char))
                       (LET ((elts (READ-DELIMITED-LIST #\} stream T)))
                         `(dict 'EQUAL ,@elts))))

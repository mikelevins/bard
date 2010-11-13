;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD-READER")


;;; ---------------------------------------------------------------------
;;; stream and sequence extensions
;;; ---------------------------------------------------------------------



;;; ---------------------------------------------------------------------
;;; the Bard reader
;;; ---------------------------------------------------------------------

(defmethod read ((in stream))
  )

(defmethod read ((ins string))
  (with-input-from-string (in ins)
    (read in)))

#|

(read "nothing")
(read "true")
(read "false")
(read "10")
(read "12.34")
(read "\\space")
(read "\\A")
(read "\\9")
(read ":Foo")
(read "Foo:")
(read "Foo:Bar")
(read "baZZ")
(read "()")
(read "[]")
(read "(0 1 2 3 4 5)")
(read "[0 1 2 3 4 5]")
(read "\"\"")
(read "\"Hello!\"")
(read "{ }")
(read "{ greeting \"Hello!\"}")


|#
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read.scm
;;;; Project:       Bard
;;;; Purpose:       bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (unit bard-reader))
(declare (uses bard-utils))



;;; (bard:read-from-string "undefined")
;;; (bard:read-from-string "nothing")
;;; (bard:read-from-string "true")
;;; (bard:read-from-string "false")
;;; (bard:read-from-string "5")
;;; (bard:read-from-string "5.4")
;;; (bard:read-from-string "5/4")
;;; (bard:read-from-string "888888888")
;;; (bard:read-from-string "#\\C")
;;; (bard:read-from-string "#\\space")
;;; (bard:read-from-string "#\\u0041")
;;; (bard:read-from-string "#\\u03BB")
;;; (bard:read-from-string "\"Fred and Barney\"")
;;; (bard:read-from-string "Fred")
;;; (bard:read-from-string "|Fred and Barney|")
;;; (bard:read-from-string "name:")
;;; (bard:read-from-string "(list 0 1 2 3)")
;;; (bard:read-from-string "[0 1 2 3]")
;;; (bard:read-from-string "{a: 1 b: 2 c: [1 2 3]}")
;;; (bard:read-from-string "{a: 1 b: [1 2 3]}")
;;; (bard:read-from-string "(^ (x) (+ x x))")
;;; (bard:read-from-string "{0 1 2 3 4 {a: 1 b: 2}}")
;;; (bard:read-from-string "{0 1 2 3 4 [01 2 3] 5 {a: 1 b: 2}}")
;;; (bard:read-from-string "{name: 'test age: 101 friends: ['a 'b 'c]}")
;;; (bard:read-from-string "{name: 'test age: 101 friends: `(,a ,b ,c)}")




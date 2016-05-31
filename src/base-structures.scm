;;;; ***********************************************************************
;;;;
;;;; Name:          base-structures.scm
;;;; Project:       Bard
;;;; Purpose:       built-in primitive structures
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************


(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; base structures 
;;; ---------------------------------------------------------------------
;;; these types are built into Gambit

;;; named literals 
;;; ---------------------------------------------------------------------

(register-structure! 'nothing (bard-structure-number-of '()))
(register-structure! 'true (bard-structure-number-of #t))
(register-structure! 'false (bard-structure-number-of #f))
(register-structure! 'end (bard-structure-number-of #!eof))
(register-structure! 'undefined (bard-structure-number-of #!unbound))

;;; Names 
;;; ---------------------------------------------------------------------

(register-structure! 'keyword (bard-structure-number-of foo:))
(register-structure! 'symbol (bard-structure-number-of 'foo))

;;;(define structure-number:uri (bard-structure-number-of (error "no definition of uri yet")))
;;;(register-structure structure-number:uri 'uri)

;;; Numbers 
;;; ---------------------------------------------------------------------

(register-structure! 'small-integer (bard-structure-number-of 1))

;;; find a bignum so we can extract its tag
(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

(register-structure! 'big-integer (bard-structure-number-of (%find-bignum)))
(register-structure! 'ratio (bard-structure-number-of 2/3))
(register-structure! 'single-float (bard-structure-number-of 2.3))

;;; Text characters 
;;; ---------------------------------------------------------------------

(register-structure! 'character (bard-structure-number-of #\A))


;;; Sequences 
;;; ---------------------------------------------------------------------

(register-structure! 'vector (bard-structure-number-of (vector 1 "two" three:)))
(register-structure! 'string (bard-structure-number-of "two"))

;;; Maps 
;;; ---------------------------------------------------------------------

(register-structure! 'hashtable (bard-structure-number-of (make-table)))

;;; Pairs 
;;; ---------------------------------------------------------------------

(register-structure! 'cons (bard-structure-number-of '(1 2 3)))

;;; Procedures 
;;; ---------------------------------------------------------------------
;;; methods
;;; functions
;;; constructors
;;; accessors

;;; Streams 
;;; ---------------------------------------------------------------------
;;; character-input-stream   
;;; character-output-stream  
;;; octet-input-stream       
;;; octet-output-stream      
;;; object-input-stream      
;;; object-output-stream     


;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       representation of basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; <alist-table>
;;; ---------------------------------------------------------------------

(define-type %alist-table
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  constructor: %private-make-alist-table
  (slots %alist-table-slots))

(define $empty-alist-table (%private-make-alist-table '()))

(define (%alist->alist-table kv-alist)
  (%private-make-alist-table (copy-alist kv-alist)))

(define (%plist->alist-table kv-plist)
  (%alist->alist-table (plist->alist kv-plist)))

(define (%alist-table-get fr key #!optional (default (%nothing)))
  (let ((slot (assoc key (%alist-table-slots fr))))
    (if slot (cadr slot) default)))

(define (%alist-table-put fr key value)
  (let* ((new-slots (append
                     (remove-if (lambda (slot)(equal? key (car slot)))
                                (%alist-table-slots fr))
                     (list (list key value)))))
    (%private-make-alist-table new-slots)))

(define (%alist-table-keys fr)(map car (%alist-table-slots fr)))
(define (%alist-table-vals fr)(map cadr (%alist-table-slots fr)))

;;; ----------------------------------------------------------------------
;;; <ascii-string>
;;; ----------------------------------------------------------------------

(define %ascii-string? string?)

;;; ----------------------------------------------------------------------
;;; <big-integer>
;;; ----------------------------------------------------------------------

(define %big-integer? ##bignum?)

;;; ----------------------------------------------------------------------
;;; <false>
;;; ----------------------------------------------------------------------

(define (%false) #f)

(define (%false? x) 
  (or (eqv? x (%false))
      (%nothing? x)))

;;; ----------------------------------------------------------------------
;;; <fixnum>
;;; ----------------------------------------------------------------------

(define %fixnum? ##fixnum?)

;;; ----------------------------------------------------------------------
;;; <flonum>
;;; ----------------------------------------------------------------------

(define (%flonum? x) (##flonum? x))

;;; ----------------------------------------------------------------------
;;; <keyword>
;;; ----------------------------------------------------------------------

(define %keyword? keyword?)

;;; ----------------------------------------------------------------------
;;; <null>
;;; ----------------------------------------------------------------------

(define (%nothing) '())
(define %nothing? %null?)

;;; ---------------------------------------------------------------------
;;; <pair>
;;; ---------------------------------------------------------------------

(define %nil '())
(define %null? null?)
(define %pair? pair?)
(define %cons cons)
(define %left car)
(define %right cdr)
(define %append append)
(define %list list)

;;; ----------------------------------------------------------------------
;;; <primitive-input-stream>
;;; ----------------------------------------------------------------------

(define %primitive-input-stream? input-port?)

;;; ----------------------------------------------------------------------
;;; <primitive-output-stream>
;;; ----------------------------------------------------------------------

(define %primitive-output-stream? output-port?)

;;; ----------------------------------------------------------------------
;;; <primitive-method>
;;; ----------------------------------------------------------------------

(define %primitive-procedure? procedure?)

;;; ----------------------------------------------------------------------
;;; <ratnum>
;;; ----------------------------------------------------------------------

(define (%ratnum? x)(##ratnum? x))

;;; ----------------------------------------------------------------------
;;; <simple-character>
;;; ----------------------------------------------------------------------

(define %simple-character? char?)

;;; ----------------------------------------------------------------------
;;; <symbol>
;;; ----------------------------------------------------------------------

(define %symbol? symbol?)

;;; ----------------------------------------------------------------------
;;; <true>
;;; ----------------------------------------------------------------------

(define (%true) #t)
(define (%true? x) 
  (and (not (%false? x))
       (not (%undefined? x))))

;;; ----------------------------------------------------------------------
;;; <undefined>
;;; ----------------------------------------------------------------------

(define (%undefined) #!unbound)
(define (%undefined? x) (or (eqv? x #!unbound)(eqv? x #!void)))
(define (%defined? x)(not (%undefined? x)))



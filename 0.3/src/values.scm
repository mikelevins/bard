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
;;; <actor>
;;; ---------------------------------------------------------------------

(define-type %actor
  id: 1C5796CD-37CD-497B-9A1C-7D4A52356BA2
  constructor: %private-make-actor
  (mailbox %actor-mailbox))

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
;;; <class>
;;; ----------------------------------------------------------------------

(define-type %class
  id: 9B7E7798-7531-4C8C-BE73-648519B355B0
  constructor: %private-make-class
  (name %class-name)
  (members %class-members %set-class-members!))

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
;;; <function>
;;; ----------------------------------------------------------------------

(define-type %function
  id: 13281261-08E9-445E-935A-B2B1D00413BB
  constructor: %private-make-function
  (name %function-name))

;;; ----------------------------------------------------------------------
;;; <keyword>
;;; ----------------------------------------------------------------------

(define %keyword? keyword?)

;;; ----------------------------------------------------------------------
;;; <method>
;;; ----------------------------------------------------------------------

(define-type %method
  id: 0D6913D2-C217-432A-A87C-A1E67CEF2919
  constructor: %private-make-method
  (lambda-list %method-lambda-list)
  (body %method-body)
  (environment %method-environment))

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
;;; <primitive-schema>
;;; ----------------------------------------------------------------------

(define-type %primitive-schema
  id: 95629F62-1E53-489A-8881-F3C3BE7F0BCC
  constructor: %private-make-primitive-schema
  (name %primitive-schema-name)
  (predicate %primitive-schema-predicate))

;;; ----------------------------------------------------------------------
;;; <protocol>
;;; ----------------------------------------------------------------------

(define-type %protocol
  id: 104759D5-467B-4EF6-9ECD-BE4DEE6350A9
  constructor: %private-make-protocol
  (name %protocol-name))

;;; ----------------------------------------------------------------------
;;; <ratnum>
;;; ----------------------------------------------------------------------

(define (%ratnum? x)(##ratnum? x))

;;; ----------------------------------------------------------------------
;;; <schema>
;;; ----------------------------------------------------------------------

(define-type %schema
  id: D1459849-11E5-47BC-ACA2-6D53C35513E1
  constructor: %private-make-schema
  (name %schema-name))

;;; ----------------------------------------------------------------------
;;; <schema-instance>
;;; ----------------------------------------------------------------------

(define-type %schema-instance
  id: 788A9504-5387-47C1-BF30-E5C1D2CBA12E
  constructor: %private-make-schema-instance
  (schema %instance-schema))

;;; ----------------------------------------------------------------------
;;; <series>
;;; ----------------------------------------------------------------------

(define-type %series
  id: 788A9504-5387-47C1-BF30-E5C1D2CBA12E
  constructor: %private-make-series
  (generator %series-generator)
  (cache %series-cache %set-series-cache!))

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




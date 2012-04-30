;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          As.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the As protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; TODO
;;; this scheme, which uses a predefined set of conversion tables, 
;;; should be replaced with proper polymorphism and singleton
;;; dispatch.

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol As)

;;; as
;;; ---------------------------------------------------------------------

(define $type-conversion-tables (make-table test: equal?))

(define (%get-conversion-table tp)
  (table-ref $type-conversion-tables tp #f))

(define (%set-conversion-table! tp tbl)
  (table-set! $type-conversion-tables tp tbl))

(define (%get-converter tbl tp)
  (table-ref tbl tp #f))

(define (%def-type-conversion t1 t2 converter)
  (let ((conversion-table (%get-conversion-table t1)))
    (if (not conversion-table)
        (begin
          (set! conversion-table (make-table test: equal?))
          (%set-conversion-table! t1 conversion-table)))
    (table-set! conversion-table t2 converter)
    converter))


(%def-type-conversion <symbol> <symbol> identity)
(%def-type-conversion <symbol> <string> (lambda (sym)(symbol->string sym)))
(%def-type-conversion <symbol> <keyword> (lambda (sym)(string->keyword (symbol->string sym))))

(%def-type-conversion <string> <string> identity)
(%def-type-conversion <string> <symbol> (lambda (str)(string->symbol str)))
(%def-type-conversion <string> <keyword> (lambda (str)(string->keyword str)))

(%def-type-conversion <keyword> <keyword> identity)
(%def-type-conversion <keyword> <string> (lambda (kw)(keyword->string kw)))
(%def-type-conversion <keyword> <symbol> (lambda (kw)(string->symbol (keyword->string kw))))

(%def-type-conversion <null> <null> identity)
(%def-type-conversion <null> <cons> identity)

(%def-type-conversion <cons> <cons> identity)
(%def-type-conversion <cons> <string> list->string)
(%def-type-conversion <cons> <frame> %list->frame)

(%def-type-conversion <string> <cons> string->list)

(%def-type-conversion <frame> <cons> %as-list)


(define bard:as (%make-function name: 'as))

(define %bard-as 
  (%primitive-method (tp val)
                     (let* ((conversion-table (%get-conversion-table (%object->bard-type val)))
                            (converter (%get-converter conversion-table tp)))
                       (if converter
                           (converter val)
                           (error (string-append "Don't know how to convert " 
                                                 (object->string (%object->bard-type val))
                                                 " to "
                                                 (object->string tp)))))))

(%function-add-method! bard:as `(,Anything ,Anything) %bard-as)

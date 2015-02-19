;;;; ***********************************************************************
;;;;
;;;; Name:          printer.scm
;;;; Project:       Bard
;;;; Purpose:       the bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (bard-class->string obj)
  (symbol->string (class-name obj)))

(define (bard-function->string obj)
  (let* ((ins (function-input-types obj))
         (instrs (interpose " " (map printer:object->string ins)))
         (outs (function-output-types obj))
         (outstrs (interpose " " (map printer:object->string outs))))
    (apply string-append
           `("(-> " ,@instrs " -> " ,@outstrs ")"))))

(define (bard-method->string obj)
  (let* ((params (method-parameters obj))
         (body (method-body obj))
         (body* (if (null? body)
                    body
                    (if (eq? 'begin (car body))
                        (cdr body)
                        body)))
         (params-str (object->string params))
         (body-str (object->string body*)))
    (apply string-append
           `("(^ " ,params-str " " ,body-str ")"))))

(define (printer:object->string obj)
  (cond
   ((eqv? obj #!void) "")
   ((class? obj) (bard-class->string obj))
   ((function? obj) (bard-function->string obj))
   ((method? obj) (bard-method->string obj))
   (else (object->string obj))))




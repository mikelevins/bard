;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          print.scm
;;;; Project:       Bard
;;;; Purpose:       printing Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/function-macros.scm")
(##include "../values/type-macros.scm")

;;;---------------------------------------------------------------------
;;; as-string
;;;---------------------------------------------------------------------

(bard:define-function bard:as-string (thing))

(bard:define-method bard:as-string ((thing Anything)) (object->string thing))
(bard:define-method bard:as-string ((thing <undefined>)) "undefined")
(bard:define-method bard:as-string ((thing <null>)) "nothing")
(bard:define-method bard:as-string ((thing <boolean>)) (if thing "true" "false"))
(bard:define-method bard:as-string ((thing <character>)) (list->string (list #\\ thing)))

(bard:define-method bard:as-string ((thing <frame>)) 
                    (let loop ((slots (bard:%frame-slots thing))
                               (outstr "{")
                               (separator ""))
                      (if (null? slots)
                          (string-append outstr "}")
                          (loop (cdr slots)
                                (let* ((slot (car slots))
                                       (k (car slot))
                                       (v (cdr slot)))
                                  (string-append outstr separator (bard:as-string k) " " (bard:as-string v)))
                                " "))))

(bard:define-method bard:as-string ((thing <closure>))
                    (if (##closure? thing)
                        (cond
                         ((%function? thing)(let* ((meta (%function-metadata thing))
                                                   (debug-name (bard:%function-debug-name meta))
                                                   (name (or (and debug-name (object->string debug-name))
                                                             "an anonymous function")))
                                              (string-append "#<function " name ">")))
                         ((%method? thing)(let* ((meta (%function-metadata thing))
                                                 (debug-name (bard:%method-debug-name meta))
                                                 (name (or (and debug-name (object->string debug-name))
                                                           "an anonymous method")))
                                            (string-append "#<method " name ">")))
                         (else (object->string thing)))
                        (object->string thing)))


(define (show x)
  (begin (newline)(display (bard:as-string x))(newline)))

;;;---------------------------------------------------------------------
;;; print
;;;---------------------------------------------------------------------

(bard:define-function bard:print-object (thing stream))

(bard:define-method bard:print-object ((thing Anything)(stream <output-stream>))
                    (print port: stream (bard:as-string thing)))


(define (bard:print x #!optional (out (current-output-port)))
  (bard:print-object x out))
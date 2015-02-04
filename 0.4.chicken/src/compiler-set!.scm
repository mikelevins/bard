;;;; ***********************************************************************
;;;;
;;;; Name:          compiler-set!.scm
;;;; Project:       Bard
;;;; Purpose:       compilation of set! forms
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------
;;; simple case
;;; (set! var val) => (ASSIGN var val)
;;; general setter
;;; (set! (car foo) val) => ((setter car) foo val)

(define (bard:compile-set! expr env)
  (if (= 3 (length expr))
      (let ((place (list-ref expr 1))
            (val (list-ref expr 2)))
        (if (symbol? place)
            (bard:compile `(ASSIGN ,place ,val) env)
            (if (= 2 (length place))
                (let ((accessor (car place))
                      (destination (cadr place)))
                  (bard:compile `((setter ,accessor) ,destination ,val) env))
                (error "malformed first argument to set!" place))))
      (error "wrong number of arguments to set!" expr)))




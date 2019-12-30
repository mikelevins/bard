;;;; ***********************************************************************
;;;;
;;;; Name:          global.scm
;;;; Project:       Bard VM
;;;; Purpose:       the global variable table
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(define +globals+ (make-table))

(define (global-ref varname) (table-ref +globals+ varname))
(define (global-set! varname val) (table-set! +globals+ varname val))

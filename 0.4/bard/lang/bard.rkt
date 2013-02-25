;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.rkt
;;;; Project:       bard 0.4
;;;; Purpose:       bard module reader
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************
#lang racket

;;; ---------------------------------------------------------------------
;;; infrastructure from racket
;;; ---------------------------------------------------------------------

(require racket/generator)

;;; ---------------------------------------------------------------------
;;; module-level syntax
;;; ---------------------------------------------------------------------

(provide (except-out (all-from-out racket) define lambda let let*)
         (except-out (all-from-out racket/generator) generator)
         (rename-out (lambda ^)
                     (define %define)
                     (generator generate)
                     (let %let)
                     (let* %let*)))

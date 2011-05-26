;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.scm
;;;; Project:       bard
;;;; Purpose:       modules and names
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; a module is a collection of names. each name in a module refers
;;; either to itself, or to an entry in another module, obtained by
;;; importing the name from the different module. A module may only
;;; import names that are flagged as exported from the originating
;;; module.

(define-type bard:name-record
  id: AB89059F-B1C0-4F8D-B44D-6036CB71D916
  constructor: bard:%make-name-record
  (name bard:%name-record-name)
  (module bard:%name-record-module)
  (exported? bard:%name-record-exported? bard:%set-name-record-exported!))

(define-type bard:module
  id: 0C626C6E-A35F-481C-A25C-BC8568580BB2
  constructor: bard:%make-module
  (names bard:module-names))

(define (bard:make-module)
  (bard:%make-module (make-table test: eq?)))

(define (bard:add-name! module name #!key (exported #f))
  (let ((rec (bard:%make-name-record name module exported)))
    (table-set! (bard:module-names module)
                name rec)
    module))

(define (bard:export-name! module name)
  (let ((rec (table-ref (bard:module-names module) name #f)))
    (if rec
        (begin
          (bard:%set-name-record-exported! rec #t)
          module)
        (bard:add-name! module name exported: #t))))

;;; all Bard module variables are cells, which are built on Termite processes
;;; in order to preserve thread safety
(define *modules* (bard:make-cell '()))
(define *module-bard.core* (bard:make-cell (bard:make-module)))
(define *module* (bard:make-cell (bard:get-cell *module-bard.core*)))

;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          module.scm
;;;; Project:       Bard VM
;;;; Purpose:       modules
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (make-module-registry)
  (make-table test: eq?))

(define current-module (make-parameter #f))

;;; ----------------------------------------------------------------------
;;; variables
;;; ----------------------------------------------------------------------

(define-type var
  id: 546917D2-A97E-4859-84E3-A9536883F7BC
  constructor: %private-make-var
  (val %private-val %private-set-val!)
  (setter %private-setter %private-set-setter!))

(define (make-var #!key (value #!unbound)(mutable #f))
  (let* ((var (%private-make-var value #f))
         (setter (if mutable
                     (lambda (x)(%private-set-val! var x))
                     #f)))
    (%private-set-setter! var setter)
    var))

;;; ----------------------------------------------------------------------
;;; modules
;;; ----------------------------------------------------------------------

(define-type module
  id: A8077BAE-5DC6-4079-9DD8-9C8C43BA9268
  constructor: %private-make-module
  read-only:
  name
  entries
  imports
  imported-modules
  exports)

(define (make-module name)
  (let ((entries (make-table test: eq?))
        (imports (make-table test: eq?))
        (imported-modules (make-table test: eq?))
        (exports (make-table test: eq?)))
    (%private-make-module name entries imports imported-modules exports)))

(define (make-import-record source-module-name source-name destname )
  (list source-module-name source-name destname))

(define (source-module import-record)
  (list-ref import-record 0))

(define (source-name import-record)
  (list-ref import-record 1))

(define (local-name import-record)
  (list-ref import-record 2))

(define (define-variable module name #!key (value #!unbound)(mutable #f))
  (if (table-ref (module-entries module) name #f)
      (error (string-append "Variable exists: " (object->string name)))
      (let ((var (make-var value: value mutable: mutable)))
        (table-set! (module-entries module) name var)
        name)))

(define (lookup-variable module varname)
  (let ((var (table-ref (module-entries module) varname #f)))
    (if var
        (%private-val var)
        $undefined)))

(define (lookup-variable-setter module varname)
  (let ((var (table-ref (module-entries module) varname #f)))
    (if var
        (%private-setter var)
        $undefined)))

(define (set-variable! module varname val)
  (let ((var (table-ref (module-entries module) varname #f)))
    (if var
        (if (%private-setter var)
            (begin
              ((%private-setter var) val)
              val)
            (error (string-append "Can't set read-only variable: " (object->string varname))))
        (error (string-append "Undefined variable: " (object->string varname))))))

(define (export-variable! module varname)
  (let ((var (table-ref (module-entries module) varname #f)))
    (if var
        (begin
          (table-set! (module-exports module) varname #t)
          varname)
        (error (string-append "Undefined variable: " (object->string varname))))))

(define (import-variable! dest-module dest-name source-module source-name )
  (if (table-ref (module-exports source-module) source-name #f)
      (let* ((var (table-ref (module-entries source-module) source-name))
             (import-record (make-import-record (module-name source-module) source-name dest-name)))
        (table-set! (module-imports dest-module) dest-name import-record)
        (table-set! (module-entries dest-module) dest-name var)
        dest-name)
      (error (string-append "Cannot import unexported variable: " (object->string varname)))))

(define (import-all! dest-module source-module)
  (let ((src-exports (module-exports source-module))
        (src-entries (module-entries source-module)))
    (table-for-each (lambda (v exported?)
                      (if exported?
                          (import-variable! dest-module v source-module v)))
                    src-exports)
    dest-module))

(define (define-module registry mname)
  (if (table-ref registry mname #f)
      (error (string-append "Module exists: " (object->string mname)))
      (let ((module (make-module mname)))
        (table-set! registry mname module)
        mname)))

(define (delete-module! registry mname)
  (if (table-ref registry mname #f)
      ;; if there's no such module, don't do any work
      (begin
        (table-for-each 
         ;; for each defined module...
         (lambda (mnm mdl)
           (if (table-ref (module-imported-modules mdl) mname #f)
               ;; if the to-be-deleted module appears in its imports table...
               (begin
                 ;; remove each imported variable...
                 (table-for-each (lambda (vnm import-record)
                                   (if (eq? mname (import-source-module import-record))
                                       ;; if the to-be-deleted module is the source of the import...
                                       (begin
                                         ;; remove the variable
                                         (table-set! (module-entries mdl) vnm)
                                         ;; remove the import record
                                         (table-set! (module-imports mdl) vnm))))
                                 (module-imports mdl))
                 ;; finally, remove the to-be-deleted module from the imports table
                 (table-set! (module-imported-modules mdl) mname))))
         registry)
        ;; last of all, discard the to-be-deleted module
        (table-set! registry mname))))

(define (list-modules registry)
  (let ((results '()))
    (table-for-each (lambda (mname module)(set! results (cons results)))
                    regsitry)
    (sort results (lambda (x y)(string<? (symbol->string x)(symbol->string y))))))

(define (get-module registry mname)
  (table-ref registry mname #f))


(define (mref module var)(lookup-variable module var))
(define (msetter module var)(lookup-variable-setter module var))

(define (find-module registry mname)
  (table-ref registry mname #f))

;;; ----------------------------------------------------------------------
;;; initial bard modules
;;; ----------------------------------------------------------------------

(define (init-bard-modules)
  (let ((modules (make-module-registry)))
    (define-module modules 'bard.lang)
    (define-module modules 'bard.user)
    (import-all! (find-module modules 'bard.user)
                 (find-module modules 'bard.lang))
    modules))


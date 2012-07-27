;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          module.scm
;;;; Project:       Bard VM
;;;; Purpose:       modules implementation
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;;
;;; module variables have global scope; they can be referenced by any
;;; code running in the VM. however, we wish to be able to compile
;;; modules separately. 
;;;
;;; in order to accomplish both goals, we establish a single global
;;; namespace for modules. a module name is a key to a global table of
;;; known modules. 
;;;
;;; each module consists of a table of variable names. a variable
;;; name, if defined, maps to a box that serves as storage for the
;;; variable.  variables can be imported by inserting a box from a
;;; different module. because an imported box may be stored under any
;;; name, this scheme enables modules to rename variables upon import

;;; ----------------------------------------------------------------------
;;; variables
;;; ----------------------------------------------------------------------

(define-type var
  id: 546917D2-A97E-4859-84E3-A9536883F7BC
  constructor: %private-make-var
  (val %private-val %private-set-val!)
  (mutable? %private-mutable?))

(define (make-var #!key (value $undefined)(mutable #f))
  (%private-make-var value mutable))

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

(define (import-source-module import-record)
  (list-ref import-record 0))

(define (import-source-name import-record)
  (list-ref import-record 1))

(define (import-local-name import-record)
  (list-ref import-record 2))

(define (define-variable module name #!key (value $undefined)(mutable #f))
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

(define (set-variable! module varname val)
  (let ((var (table-ref (module-entries module) varname #f)))
    (if var
        (if (%private-mutable? var)
            (begin
              (%private-set-val! var val)
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

(define (import-variable! source-module source-name dest-module dest-name)
  (if (table-ref (module-exports source-module) source-name #f)
      (let* ((var (table-ref (module-entries source-module) source-name))
             (import-record (make-import-record (module-name source-module) source-name dest-name)))
        (table-set! (module-imports dest-module) dest-name import-record)
        (table-set! (module-entries dest-module) dest-name var)
        dest-name)
      (error (string-append "Cannot import unexported variable: " (object->string varname)))))

;;; ----------------------------------------------------------------------
;;; the module registry
;;; ----------------------------------------------------------------------

(define *the-module-registry* (make-table test: eq?))

(define (define-module mname)
  (if (table-ref *the-module-registry* mname #f)
      (error (string-append "Module exists: " (object->string mname)))
      (let ((module (make-module mname)))
        (table-set! *the-module-registry* mname module)
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
         *the-module-registry*)
        ;; last of all, discard the to-be-deleted module
        (table-set! *the-module-registry* mname))))

(define (list-modules)
  (let ((results '()))
    (table-for-each (lambda (mname module)(set! results (cons results)))
                    *the-module-registry*)
    (sort results (lambda (x y)(string<? (symbol->string x)(symbol->string y))))))

(define (find-module registry mname)
  (table-ref registry mname #f))

(define (mref registry mname varname)
  (let ((mdl (find-module registry mname)))
    (if mdl
        (lookup-variable mdl varname)
        #!unbound)))

(define (mset! registry mname varname val)
  (let ((mdl (find-module registry mname)))
    (if mdl
        (set-variable! mdl varname val)
        (error (string-append "No such module: " (object->string mname))))
    val))

(define-module 'bard.lang)
(define-module 'bard.user)

(define (%bard-modules)
  *the-module-registry*)

(define (%default-initial-module)
  (find-module (%bard-modules) 'bard.user))

;;; ----------------------------------------------------------------------
;;; module names
;;; ----------------------------------------------------------------------
;;; Bard symbol syntax is:
;;;   <module-name>:<symbol-name>
;;; An empty module name is interpreted as a reference to
;;; the module named 'bard.keyword', so 
;;;    :foo
;;; is read as
;;;    bard.keyword:foo
;;; All symbols in the bard.keyword module are considered keywords
;;; by definition, and always evaluate to themselves.
;;;
;;; A symbol written with no qualifying module name and no colon
;;; is read as a reference to the symbol's name in the current
;;; module. The current module is the value of bard.lang:*module*
;;; in the current read context.
;;;
;;; When Bard starts running, the current module is bard.user.
;;; Then the reader encounters an (in-module ...) form, it
;;; updates the binding of bard.land:*module* to refer to the
;;; named module. Thus:
;;;
;;; ; code here is in bard.user
;;;
;;; (in-module bard.lang)
;;;
;;; ; code here is in bard.lang
;;;
;;; (in-module foo.bar)
;;;
;;; ; code here is in foo.bar
;;;
;;; Module names must begin with an alphabetic character. After
;;; the initial alphabetic character, names may contain any
;;; non-whitespace characters except these:  :;()[]{},'"`
;;; Names are conventionally formatted similarly to 
;;; the reverse domain names commonly used to name Java packages,
;;; thus:
;;;
;;;   bard.lang
;;;   bard.user
;;;   com.habilis.nelson
;;;
;;; when Bard reads a symbol, it searches for a ':' and splits the
;;; symbol's name at that point; the segment to the left of the ':' is
;;; the module name; the segment to the right is the variable name.
;;; if there is no colon, the current module name is inferred. If a
;;; colon is the leftmost character in the name, then the module
;;; bard.keyword is inferred.
;;;
;;; it is an error for more than one colon to appear in a name.

(define (colon-position s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (< i len)
          (if (char=? #\: (string-ref s i))
              i
              (loop (+ i 1)))
          #f))))

(define (parse-symbol-name s)
  (if (keyword? s)
      (values (keyword->string s) #f)
      (let* ((str (symbol->string s))
             (colon-pos (colon-position str)))
        (if colon-pos
            (values (substring str (+ 1 colon-pos)(string-length str))
                    (substring str 0 colon-pos))
            (values str #f)))))


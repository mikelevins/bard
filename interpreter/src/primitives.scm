;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prims.scm
;;;; Project:       Bard
;;;; Purpose:       primitive procedures, defined in Scheme, but bound to
;;;;                Bard variables in the initial environment
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; repl and I/O
;;; ---------------------------------------------------------------------

(define prim:read
  (make-primitive
   procedure: (lambda (#!optional (in (current-input-port)))(bard:read in))
   debug-name: 'read
   required-count: 0
   restarg: 'more))

(define prim:read-text
  (make-primitive
   procedure: (lambda (text)(bard:read-from-string text))
   debug-name: 'read-text
   required-count: 1
   restarg: #f))

(define (%bard-read-file path)
  (call-with-input-file path
    (lambda (in)
      (let* ((finfo (file-info path))
             (fsize (file-info-size finfo))
             (buf (make-string fsize)))
        (read-substring buf 0 fsize in)
        buf))))

(define prim:read-file
  (make-primitive
   procedure: %bard-read-file
   debug-name: 'read-file
   required-count: 1
   restarg: #f))

(define prim:eval
  (make-primitive
   procedure: (lambda (expr #!optional (env (%null-environment)))
                (%eval expr env))
   debug-name: 'eval
   required-count: 1
   restarg: 'more))

(define prim:apply
  (make-primitive
   procedure: (lambda (fn args)(%apply fn args))
   debug-name: 'apply
   required-count: 2
   restarg: #f))

(define prim:print
  (make-primitive
   procedure: (lambda (thing #!optional (out (current-output-port)))
                (bard:print thing out))
   debug-name: 'print
   required-count: 1
   restarg: 'more))

(define prim:newline
  (make-primitive
   procedure: (lambda ()(newline))
   debug-name: 'newline
   required-count: 0
   restarg: #f))

(define prim:load
  (make-primitive
   procedure: (lambda (path)(%bard-load path))
   debug-name: 'load
   required-count: 1
   restarg: #f))

;;; Number
;;; ---------------------------------------------------------------------

(define prim:+
  (make-primitive
   procedure: +
   debug-name: '+
   required-count: 0
   restarg: 'more))

(define prim:-
  (make-primitive
   procedure: -
   debug-name: '-
   required-count: 0
   restarg: 'more))

(define prim:*
  (make-primitive
   procedure: *
   debug-name: '*
   required-count: 0
   restarg: 'more))

(define prim:/
  (make-primitive
   procedure: /
   debug-name: '/
   required-count: 0
   restarg: 'more))

(define prim:=
  (make-primitive
   procedure: =
   debug-name: '=
   required-count: 0
   restarg: 'more))

(define prim:>
  (make-primitive
   procedure: >
   debug-name: '>
   required-count: 0
   restarg: 'more))

(define prim:<
  (make-primitive
   procedure: <
   debug-name: '<
   required-count: 0
   restarg: 'more))

(define prim:>=
  (make-primitive
   procedure: >=
   debug-name: '>=
   required-count: 0
   restarg: 'more))

(define prim:<=
  (make-primitive
   procedure: <=
   debug-name: '<=
   required-count: 0
   restarg: 'more))

(define prim:odd?
  (make-primitive
   procedure: odd?
   debug-name: 'odd?
   required-count: 1
   restarg: #f))

(define prim:even?
  (make-primitive
   procedure: even?
   debug-name: 'even?
   required-count: 1
   restarg: #f))

(define prim:random
  (make-primitive
   procedure:
   (let ((rs (make-random-source)))
     (random-source-randomize! rs)
     (let ((ri (random-source-make-integers rs)))
       (lambda (n)(ri n))))
   debug-name: 'random
   required-count: 1
   restarg: #f))

;;; List
;;; ---------------------------------------------------------------------

(define prim:list
  (make-primitive
   procedure: (lambda args args)
   debug-name: 'list
   required-count: 0
   restarg: 'more))

;;; Table
;;; ---------------------------------------------------------------------

(define prim:table
  (make-primitive
   procedure: (lambda args (%make-alist-table (plist->alist args)))
   debug-name: 'table
   required-count: 0
   restarg: 'more))

;;; System 
;;; ---------------------------------------------------------------------

(define prim:gc
  (make-primitive
   procedure: ##gc
   debug-name: 'gc
   required-count: 0
   restarg: #f))

(define prim:gensym
  (make-primitive
   procedure: (lambda ()(gensym))
   debug-name: 'gensym
   required-count: 0
   restarg: #f))

(define prim:room
  (make-primitive
   procedure: (lambda ()
                (begin
                  (gc-report-set! #t)
                  (##gc)
                  (gc-report-set! #f)))
   debug-name: 'room
   required-count: 0
   restarg: #f))

(define prim:error
  (make-primitive
   procedure: (lambda (msg)
                (let ((msg (if (string? msg)
                               msg
                               (object->string msg))))
                  (display (string-append "ERROR: " msg))))
   debug-name: 'error
   required-count: 1
   restarg: #f))

(define prim:exit
  (make-primitive
   procedure: exit
   debug-name: 'exit
   required-count: 0
   restarg: #f))

(define prim:quit
  (make-primitive
   procedure: exit
   debug-name: 'quit
   required-count: 0
   restarg: #f))

(define prim:version
  (make-primitive
   procedure: (lambda () $bard-version-string)
   debug-name: 'version
   required-count: 0
   restarg: #f))

(define prim:uuid
  (make-primitive
   procedure: (lambda () (make-uuid))
   debug-name: 'uuid
   required-count: 0
   restarg: #f))

;;; functions
;;; ---------------------------------------------------------------------

(define prim:identity
  (make-primitive
   procedure: (lambda (x) x)
   debug-name: 'identity
   required-count: 1
   restarg: #f))

(define prim:complement 
  (make-primitive
   procedure: (lambda (f)
                (make-primitive
                 procedure: (lambda args (not (%apply f args)))
                 debug-name: #f
                 required-count: 0
                 restarg: 'more))
   debug-name: 'complement
   required-count: 1
   restarg: #f))

(define prim:constantly 
  (make-primitive
   procedure:
   (lambda (c)
     (make-interpreted-method
      body: `(begin ,c)
      debug-name: #f
      formal-parameters: '(& more)
      environment: (%null-enviroment)
      restarg: 'more))
   debug-name: 'constantly
   required-count: 1
   restarg: #f))

(define prim:flip 
  (make-primitive
   procedure: (lambda (f)
                (make-primitive procedure:
                                (lambda (x y) (%apply f (list y x)))
                                debug-name: #f
                                required-count: 2
                                restarg: #f))
   debug-name: 'flip
   required-count: 1
   restarg: #f))

(define prim:partial 
  (make-primitive
   procedure: (lambda (f #!rest outer-args)
                (make-primitive
                 procedure: (lambda inner-args (%apply f (append outer-args inner-args)))
                 debug-name: #f
                 required-count: 0
                 restarg: 'more))
   debug-name: 'partial
   required-count: 1
   restarg: 'more))

;;; type system
;;; ---------------------------------------------------------------------

(define prim:singleton?
  (make-primitive
   procedure: %singleton?
   debug-name: 'singleton?
   required-count: 1
   restarg: #f))

(define prim:singleton
  (make-primitive
   procedure: %singleton
   debug-name: 'singleton
   required-count: 1
   restarg: #f))



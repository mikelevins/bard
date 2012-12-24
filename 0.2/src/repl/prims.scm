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
  (%make-primitive-method
   (lambda (#!optional (in (current-input-port)))
     (bard:read in))
   name: 'read
   parameters: %nil
   required-count: 0
   restarg: 'more))

(define prim:read-text
  (%make-primitive-method
   (lambda (text)
     (bard:read-from-string text))
   name: 'read-text
   parameters: (%list 'text)
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
  (%make-primitive-method
   %bard-read-file
   name: 'read-file
   parameters: (%list 'path)
   required-count: 1
   restarg: #f))

(define prim:eval
  (%make-primitive-method
   (lambda (expr #!optional (env (%null-environment)))
     (%eval expr env))
   name: 'eval
   parameters: (%list 'expr)
   required-count: 1
   restarg: 'more))


(define prim:apply
  (%make-primitive-method
   (lambda (fn args)(%apply fn args))
   name: 'apply
   parameters: (%list 'fn 'args)
   required-count: 2
   restarg: #f))

(define prim:next
  (%make-primitive-method
   (lambda (gen)(%next gen))
   name: 'next
   parameters: (%list 'gen)
   required-count: 1
   restarg: #f))

(define prim:print
  (%make-primitive-method
   (lambda (thing #!optional (out (current-output-port)))
     (bard:print thing out))
   name: 'print
   parameters: (%list 'thing)
   required-count: 1
   restarg: 'more
   ))

(define prim:newline
  (%make-primitive-method
   (lambda ()(newline))
   name: 'newline
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

(define prim:load
  (%make-primitive-method
   (lambda (path)(%bard-load path))
   name: 'load
   parameters: (%list 'path)
   required-count: 1
   restarg: #f
   ))

;;; Number
;;; ---------------------------------------------------------------------

(define prim:+
  (%make-primitive-method
   +
   name: '+
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:-
  (%make-primitive-method
   -
   name: '-
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:*
  (%make-primitive-method
   *
   name: '*
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:/
  (%make-primitive-method
   /
   name: '/
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:=
  (%make-primitive-method
   =
   name: '=
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:>
  (%make-primitive-method
   >
   name: '>
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:<
  (%make-primitive-method
   <
   name: '<
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:>=
  (%make-primitive-method
   >=
   name: '>=
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:<=
  (%make-primitive-method
   <=
   name: '<=
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

(define prim:odd?
  (%make-primitive-method
   odd?
   name: 'odd?
   parameters: (%list 'n)
   required-count: 1
   restarg: #f
   ))

(define prim:even?
  (%make-primitive-method
   even?
   name: 'even?
   parameters: (%list 'n)
   required-count: 1
   restarg: #f
   ))

(define prim:random
  (%make-primitive-method
   random-integer
   name: 'random
   parameters: (%list 'n)
   required-count: 1
   restarg: #f
   ))

;;; List
;;; ---------------------------------------------------------------------

(define prim:list
  (%make-primitive-method
   (lambda args (%cons->bard-list args))
   name: 'list
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

;;; Table
;;; ---------------------------------------------------------------------

(define prim:table
  (%make-primitive-method
   (lambda args (%make-table (%cons->bard-list args)))
   name: 'table
   parameters: %nil
   required-count: 0
   restarg: 'more
   ))

;;; System 
;;; ---------------------------------------------------------------------

(define prim:gc
  (%make-primitive-method
   ##gc
   name: 'gc
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

(define prim:room
  (%make-primitive-method
   (lambda ()
     (begin
       (gc-report-set! #t)
       (##gc)
       (gc-report-set! #f)))
   name: 'room
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

(define prim:error
  (%make-primitive-method
   (lambda (msg)
     (let ((msg (if (string? msg)
                    msg
                    (object->string msg))))
       (display (string-append "ERROR: " msg))))
   name: 'error
   parameters: '(err)
   required-count: 1
   restarg: #f
   ))

(define prim:exit
  (%make-primitive-method
   exit
   name: 'exit
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

(define prim:quit
  (%make-primitive-method
   exit
   name: 'quit
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

(define prim:version
  (%make-primitive-method
   (lambda () $bard-version-string)
   name: 'version
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

(define prim:uuid
  (%make-primitive-method
   (lambda () (make-uuid))
   name: 'uuid
   parameters: %nil
   required-count: 0
   restarg: #f
   ))

;;; functions
;;; ---------------------------------------------------------------------

(define prim:identity
  (%make-primitive-method
   (lambda (x) x)
   name: 'identity
   parameters: (%list 'x)
   required-count: 1
   restarg: #f
   ))

(define prim:complement 
  (%make-primitive-method
   (lambda (f)
     (%make-primitive-method (lambda args (not (%apply f (%cons->bard-list args))))
                             name: #f
                             parameters: %nil
                             required-count: 0
                             restarg: 'more
                             ))
   name: 'complement
   parameters: (%list 'fn)
   required-count: 1
   restarg: #f
   ))

(define prim:constantly 
  (%make-primitive-method
   (lambda (c)
     (%make-primitive-method (lambda args c)
                             name: #f
                             parameters: %nil
                             required-count: 0
                             restarg: 'more
                             ))
   name: 'constantly
   parameters: (%list 'x)
   required-count: 1
   restarg: #f
   ))

(define prim:flip 
  (%make-primitive-method
   (lambda (f)
     (%make-primitive-method (lambda (x y) (%apply f (%list y x)))
                             name: #f
                             parameters: (%list 'x 'y)
                             required-count: 2
                             restarg: #f
                             ))
   name: 'flip
   parameters: (%list 'fn)
   required-count: 1
   restarg: #f
   ))

(define prim:partial 
  (%make-primitive-method
   (lambda (f #!rest outer-args)
     (%make-primitive-method (lambda inner-args (%apply f (%cons->bard-list (append outer-args inner-args))))
                             name: #f
                             parameters: %nil
                             required-count: 0
                             restarg: 'more
                             ))
   name: 'partial
   parameters: (%list 'fn)
   required-count: 1
   restarg: 'more
   ))

;;; type system
;;; ---------------------------------------------------------------------

(define prim:type?
  (%make-primitive-method
   %type?
   name: 'type?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f
   ))

(define prim:type
  (%make-primitive-method
   %object->bard-type
   name: 'type
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f
   ))

(define prim:instance-of?
  (%make-primitive-method
   %instance-of?
   name: 'instance-of?
   parameters: (%list 'thing 'tp)
   required-count: 2
   restarg: #f
   ))

(define prim:singleton?
  (%make-primitive-method
   %singleton?
   name: 'singleton?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f
   ))

(define prim:singleton
  (%make-primitive-method
   %singleton
   name: 'singleton
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f
   ))

(define prim:make
  (%make-primitive-method
   %make
   name: 'make
   parameters: (%list )
   required-count: 1
   restarg: #t))

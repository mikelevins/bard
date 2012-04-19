;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.scm
;;;; Project:       Bard
;;;; Purpose:       a read-eval-print loop for Bard
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%initial-bard-environment)
  (%extend-environment 
   (%null-environment)
   (list
    ;; generic comparison
    '= bard:=
    ;; numbers
    '+ prim:+
    '- prim:-
    '* prim:*
    '/ prim:/
    '> prim:>
    '< prim:<
    '>= prim:>=
    '<= prim:<=
    ;; List
    'list prim:list
    ;; Frame
    'frame prim:frame
    ;; representations
    '<undefined> <undefined>
    '<null> <null>
    '<character> <character>
    '<boolean> <boolean>
    '<symbol> <symbol>
    '<keyword> <keyword>
    '<flonum> <flonum>
    '<ratnum> <ratnum>
    '<fixnum> <fixnum>
    '<bignum> <bignum>
    '<primitive-procedure> <primitive-procedure>
    '<cons> <cons>
    '<string> <string>
    '<input-stream> <input-stream>
    '<output-stream> <output-stream>
    '<frame> <frame>
    '<function> <function>
    '<method> <method>
    ;; protocols
    'Anything Anything
    'List List
    'add-first bard:add-first
    'add-last bard:add-last
    'any bard:any
    'append bard:append
    'contains? bard:contains?
    )))


(define *bard-banner* "Bard 0.1")
(define *bard-prompt* "bard> ")

(define (bard:repl)
  (set! $bard-toplevel-environment (%initial-bard-environment))
  (newline)
  (display *bard-banner*)
  (newline)
  (read-line)
  (let loop ()
    (newline)
    (display *bard-prompt*)
    (let* ((input (read-line))
           (expr (bard:read-from-string input)))
      (if (or (eq? expr quit:)
              (eq? expr q:))
          (begin
            (newline)
            (display "Bard terminated")
            (newline))
          (if (eq? expr #!eof)
              (loop)
              (let* ((val (%eval expr $bard-toplevel-environment))
                     (valstr (%as-string val)))
                (newline)
                (display valstr)
                (loop)))))))


;;; (%eval '((method (x)(+ x 1)) 2) (%initial-bard-environment))
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
    ;; general
    'id (lambda (x) x)
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
    'odd? odd?
    'even? even?
    'random (lambda (n)(random-integer n))
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
    'difference bard:difference
    'drop bard:drop
    'drop-before bard:drop-before
    'element bard:element
    'empty? bard:empty?
    'every? bard:every?
    'filter bard:filter
    'find bard:find
    'first bard:first
    'interleave bard:interleave
    'interpose bard:interpose
    'intersection bard:intersection
    'last bard:last
    'length bard:length
    'list? bard:list?
    'map bard:map
    'position bard:position
    'range bard:range
    'reduce bard:reduce
    'repeat bard:repeat
    'reverse bard:reverse
    'second bard:second
    'select bard:select
    'shuffle bard:shuffle
    'slice bard:slice
    'some? bard:some?
    'sort bard:sort
    'tail bard:tail
    'tails bard:tails
    'take bard:take
    'take-before bard:take-before
    'unique bard:unique
    'unzip bard:unzip
    'zip bard:zip
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
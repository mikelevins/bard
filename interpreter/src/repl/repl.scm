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
   `(;; generic comparison
     = ,bard:=
     ;; numbers
     + ,prim:+
     - ,prim:-
     * ,prim:*
     / ,prim:/
     > ,prim:>
     < ,prim:<
     >= ,prim:>=
     <= ,prim:<=
     )))


(define (bard:rep str env)
  (bard:print (%eval (bard:read-from-string str) env)))

;;; (bard:rep "+" (%initial-bard-environment))
;;; (bard:rep "(+ 2 3)" (%initial-bard-environment))
;;; (bard:rep "(- 2 3)" (%initial-bard-environment))
;;; (bard:rep "(* 2 3)" (%initial-bard-environment))
;;; (bard:rep "(/ 2 3)" (%initial-bard-environment))
;;; (bard:rep "(= 2 3)" (%initial-bard-environment))
;;; (bard:rep "(= \"Fred\" \"Fred\")" (%initial-bard-environment))
;;; (bard:rep "(= \"Fred\" 5)" (%initial-bard-environment))
;;; (bard:rep "(> 2 3)" (%initial-bard-environment))
;;; (bard:rep "(< 2 3)" (%initial-bard-environment))
;;; (bard:rep "(>= 2 3)" (%initial-bard-environment))
;;; (bard:rep "(<= 2 3)" (%initial-bard-environment))

(define *bard-banner* "Bard 0.1")
(define *bard-prompt* "bard> ")

(define (bard:repl)
  (set! $bard-toplevel-environment (%initial-bard-environment))
  (newline)
  (display *bard-banner*)
  (newline)
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


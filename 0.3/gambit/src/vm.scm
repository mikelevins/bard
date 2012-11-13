;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define $bard-version-string "Bard version 0.3.0")

(define-type %vm
  constructor: %private-make-vm
  (fn %fn %setfn!)
  (code %code %setcode!)
  (pc %pc %setpc!)
  (instr %instr %setinstr!)
  (stack %stack %setstack!)
  (nargs %nargs %setnargs!)
  (env %env %setenv!)
  (globals %globals))

(define (%makevm)
  (let ((code (%null-code))
        (pc 0)
        (instr NOP)
        (stack '())
        (nargs 0)
        (env (%null-env))
        (globals (%bard-globals))
        (instructions (make-table test: eq?)))
    (%private-make-vm code pc instr stack nargs env globals stepfn logfn instructions)))

(define (%push! vm val)(%setstack! vm (cons val (%stack vm))))

(define (%pop! vm)
  (let ((val (car (%stack vm))))
    (%setstack! vm (cdr (%stack vm)))
    val))

(define (%top vm)(car (%stack vm)))

(define (%init-globals itable) #f)
(define (%setstepfn! vm stepfun) #f)
(define (%vmfetch! vm)(%setinstr! vm (%code-ref (%code vm)(%pc vm))))
(define (%vmincpc! vm)(%setpc! vm (+ 1 (%pc vm))))
(define (%vmexec! vm)((%op (%instr vm))))

(define (%stepvm vm)
  (%vmfetch! vm)
  (%vmincpc! vm)
  (%vmexec! vm))

(define (%showvm vm)
  #f)

(define (%step&show vm)
  (%stepvm vm)
  (%showvm vm))

(define (%load vm code)(%setcode! vm (%link vm code)))

(define (%runvm vm #!key (log #f))
  (if (%vm-code vm)
      (call/cc
       (lambda (exit)
         (%defop 'HALT (lambda (m)(exit (%top m))))
         (let loop ()
           (%stepvm vm)
           (loop))))
      (error (str "No program loaded in VM: " vm))))

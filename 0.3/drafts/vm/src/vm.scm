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

(define-type vmstate
  constructor: %vmstate
  extender: defstate
  (fn %fn %setfn!)
  (pc %pc %setpc!)
  (env %env %setenv!))

(defstate vm
  constructor: %private-make-vm
  (code %code %setcode!)
  (instr %instr %setinstr!)
  (nargs %nargs %setnargs!)
  (vals %vals %setvals!)
  (stack %stack %setstack!)
  (globals %globals %setglobals!))

(define (%makevm #!key (fn #f)(env (%null-env))(globals (%bard-globals)))
  (let ((code #f)
        (pc 0)
        (instr #f)
        (nargs 0)
        (vals '())
        (stack '())
        (env env)
        (globals globals))
    (%private-make-vm fn code pc instr nargs vals stack env globals)))

(define (%loadfn vm fn)
  (cond
   ((%function? fn)(%load-function vm fn))
   ((%serialized-function? fn)(%load-function vm (%deserialize-function vm fn)))
   ((%method? fn)(%load-method vm fn))
   ((%serialized-method? fn)(%load-method vm (%deserialize-method vm fn)))
   (else (error (str "Tried to load a function, but found a value that was not a function:" fn)))))

(define (%initvm vm)
  (if (%fn vm)
      (begin
        (%setcode! vm (%fn-code (%fn vm)))
        (%setpc! vm 0)
        vm)
      (error (str "No function loaded"))))

(define (%fetch! vm)(%setinstr! vm (%code-ref (%code vm)(%pc vm))))
(define (%incpc! vm)(%setpc! vm (+ 1 (%pc vm))))
(define (%exec! vm)
  (receive (op args)(%decode (%instr vm))
           (apply op vm args)))


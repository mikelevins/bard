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

;;; ---------------------------------------------------------------------
;;; vm data structure
;;; ---------------------------------------------------------------------

(define-type vmstate
  constructor: %vmstate
  extender: defstate
  (fn %fn %setfn!)
  (pc %pc %setpc!)
  (env %env %setenv!))

(defstate vm
  constructor: %private-make-vm
  (instr %instr %setinstr!)
  (vals %vals %setvals!)
  (stack %stack %setstack!)
  (globals %globals %setglobals!)
  (initfn %initfn %setinitfn!)
  (exitfn %exitfn %setexitfn!)
  (stepfn %stepfn %setstepfn!)
  (runfn %runfn %setrunfn!)
  (prompt %prompt %setprompt!)
  (shellfn %shellfn %setshellfn!))

;;; ---------------------------------------------------------------------
;;; vm operations
;;; ---------------------------------------------------------------------

(define (%push! v vm)(%setstack! vm (cons v (%stack vm))))
(define (%top vm)(car (%stack vm)))
(define (%pop! vm)(prog1 (%top vm)(%setstack! vm (cdr (%stack vm)))))

(define (%pushv! v vm)(%setvals! vm (cons v (%vals vm))))
(define (%topv vm)(car (%vals vm)))
(define (%popv! vm)(prog1 (%topv vm)(%setvals! vm (cdr (%vals vm)))))

(define (%make-saved vm pc)(%vmstate (%fn vm) pc (%env vm)))

(define (%showvm vm)
  (newline)
  (display (str "Bard VM v" $bard-version-string)))

(define (%promptread vm)
  (newline)
  (display (%prompt vm))
  (read-line))

(define (%handlevmcmd vm cmdstr)
  (let ((cmd-line (words (ltrim cmdstr))))
    (if (null? cmd-line)
        (values)
        (let ((cmd (car cmd-line))
              (args (cdr cmd-line)))
          (cond
           ((string=? "show" cmd)(%showvm vm))
           ((string=? "quit" cmd)(%exitvm vm))
           (else (error (str "Unrecognized VM command: " cmd))))))))

;;; ---------------------------------------------------------------------
;;; vm constructor
;;; ---------------------------------------------------------------------

(define (%makevm #!key (fn #f)(env (%null-env))(globals (%bard-globals)))
  (let* ((vm (%private-make-vm fn 0 env #f '() '() globals #f #f #f #f "bard> " #f)))
    (%setinitfn! vm (lambda ()
                      (call/cc
                       (lambda (exit)
                         (letrec ((_fetch! (lambda ()(%setinstr! vm (%code-ref (%method-code (%fn vm))(%pc vm)))))
                                  (_inc! (lambda ()(%setpc! vm (+ 1 (%pc vm)))))
                                  (_exec! (lambda ()(receive (op args)(%decode (%instr vm))
                                                        (op vm args)))))
                           (%setexitfn! vm exit)
                           (%setstepfn! vm (lambda ()(_fetch!)(_inc!)(_exec!)))
                           (%setrunfn! vm (lambda ()(let loop ()(_fetch!)(_inc!)(_exec!)(loop))))
                           (%setshellfn! vm (lambda ()(let loop ()(%handlevmcmd vm (%promptread vm))(loop)))))))))
    vm))

;;; ---------------------------------------------------------------------
;;; vm control
;;; ---------------------------------------------------------------------

(define (%initvm! vm)
  ((%initfn vm))
  vm)

(define (%exitvm vm)
  ((%exitfn vm))
  vm)

(define (%step! vm)
  ((%stepfn vm))
  vm)

(define (%startvm vm)
  ((%shellfn vm))
  vm)


;;; (define $fn (%make-method '() (%assemble (%compile '(fx+ 2 3) '() #t #t))))
;;; (define $vm (%makevm fn: $fn))
;;; (%initvm! $vm)
;;; (%startvm $vm)




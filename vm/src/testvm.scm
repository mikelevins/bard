(define (testvm program)
  (let* ((env (make-env '()))
         (fn (make-fn 'testfn program env))
         (vm (make-vm '() $fn program 0 env '() 0 #f #f)))
    (display-vm-status vm)
    (time (runvm vm))))

;;; run $code3 from bardvm.scm
(testvm $code3)


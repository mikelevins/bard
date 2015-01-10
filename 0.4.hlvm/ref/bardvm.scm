;;; ---------------------------------------------------------------------
;;; bardvm
;;; a simple virtual machine for a lisp-lke language
;;; copyright 2015 by mikel evins
;;; ---------------------------------------------------------------------

(define *version* "0.4.0")
(define *banner* (string-append "bardvm " *version*))

;;; ---------------------------------------------------------------------
;;; programs
;;; ---------------------------------------------------------------------



(define (valid-program-path? program-path)
  )

(define (read-program-file program-path)
  )

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (null-environment)
  )

;;; ---------------------------------------------------------------------
;;; globals
;;; ---------------------------------------------------------------------

(define (standard-globals)
  )

;;; ---------------------------------------------------------------------
;;; the vm data structure
;;; ---------------------------------------------------------------------

(define-structure vm
  pc fn code instruction halted?
  env globals stack nargs)

(define (construct-vm program)
  (make-vm 0 ; pc
           (program-fn program) ; fn
           (fn-code (program-fn program)) ; code
           #f ; instruction
           #f ; halted?
           (null-environment) ; env
           (standard-globals) ; globals
           '() ; stack
           0 ; nargs
           ))

;;; ---------------------------------------------------------------------
;;; starting and running the vm
;;; ---------------------------------------------------------------------

(define (exit! vm)
  (display vm))

(define (fetch! vm)
  (vm-instruction-set! vm
                       (code-ref (vm-code vm)
                                 (vm-pc vm))))

(define (incpc! vm)
  )

(define (exec! vm)
  )

(define (step! vm)
  (fetch! vm)
  (incpcp! vm)
  (exec! vm))

(define (vmstart vm)
  (newline)
  (display *banner*)
  (newline)
  (let loop ()
    (if (vm-halted? vm)
        (exit! vm)
        (begin (step! vm)
               (loop)))))

;;; ---------------------------------------------------------------------
;;; main entry point
;;; ---------------------------------------------------------------------

(define (bardrun)
  (let* ((args (cdr (command-line)))
         (program-path (if (null? args)
                           #f
                           (car args))))
    (if (valid-program-path? program-path)
        (let* ((program (read-program-file program-path))
               (vm (construct-vm program)))
          (vmstart vm))
        (error "Invalid program path" program-path))))

(bardrun)

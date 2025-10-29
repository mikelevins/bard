
(in-package :bard)


(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

(defstruct (vm (:conc-name vm-) (:constructor %make-vm))
  "Stack-based bytecode VM with closures and continuations."
  (fn nil)
  (code (make-array 100 :fill-pointer 0 :adjustable t))
  (pc 0)
  (env nil)
  (stack (make-array 1000 :fill-pointer 0 :adjustable t))
  (nargs 0)
  (instr nil))

#+repl (setf $vm (%make-vm))

;;; the toplevel monitor

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; Monitor Commands
;;; ---------------------------------------------------------------------

(defun cmd-list ()
  "List all VMs"
  (format t "~%cmd-list: List all VMs" ))

(defun cmd-compile (expr)
  "Compile bard expression to bytecode"
  (format t "~%cmd-compile EXPR: Compile bard expression to bytecode" ))

(defun cmd-decompile (name)
  "Decompile VM code (alias for disassemble)"
  (format t "~%cmd-decompile NAME: Decompile VM code (alias for disassemble)" ))

(defun cmd-help ()
  "Display help"
  (format t "~%Available commands:~%")
  #|
  (format t "  (load-bytecode FILENAME [NAME])     Load bytecode file~%")
  (format t "  (load-state FILENAME [NAME])        Load saved VM state~%")
  (format t "  (run NAME)                          Start VM in thread~%")
  (format t "  (stop NAME)                         Stop VM thread~%")
  (format t "  (step NAME [N])                     Execute N steps~%")
  (format t "  (inspect NAME)                      Show VM state~%")
  (format t "  (disassemble NAME)                  Show VM code~%")
  (format t "  (inspect-closure NAME)              Inspect closure in FN~%")
  (format t "  (save-state NAME FILENAME)          Save VM to file~%")
  |#
  (format t "  (list)                              List all VMs~%")
  (format t "  (compile EXPR)                      Compile bard expression~%")
  (format t "  (decompile NAME)                    Decompile VM code~%")
  (format t "  (help)                              This help~%")
  (format t "  (quit)                              Exit monitor~%")
  (format t "~%"))



;;; ---------------------------------------------------------------------
;;; Monitor REPL
;;; ---------------------------------------------------------------------

(defun read-monitor-command ()
  "Read a command from the monitor REPL"
  (format t "~%monitor> ")
  (force-output)
  (let* ((*package* (find-package :bard))
         (*read-eval* nil))
    (read)))

(defun eval-monitor-command (cmd)
  "Evaluate a monitor command"
  (handler-case
      (cond
        ((atom cmd)
         (case cmd
           (quit :quit)
           (list (cmd-list))
           (help (cmd-help))
           (t (format t "Unknown command: ~a~%" cmd))))
        
        ((consp cmd)
         (case (first cmd)
           #|
           (load-bytecode (apply #'cmd-load-bytecode (rest cmd)))
           (load-image (apply #'cmd-load-image (rest cmd)))
           (run (apply #'cmd-run (rest cmd)))
           (stop (apply #'cmd-stop (rest cmd)))
           (step (apply #'cmd-step (rest cmd)))
           (inspect (apply #'cmd-inspect (rest cmd)))
           (disassemble (apply #'cmd-disassemble (rest cmd)))
           (inspect-closure (apply #'cmd-inspect-closure (rest cmd)))
           (save-image (apply #'cmd-save-image (rest cmd)))
           |#
           (list (cmd-list))
           (compile (cmd-compile (second cmd)))
           (decompile (apply #'cmd-decompile (rest cmd)))
           (help (cmd-help))
           (quit :quit)
           (t (format t "Unknown command: ~a~%" (first cmd)))))
        
        (t (format t "Invalid command~%")))
    (error (e)
      (format t "Error: ~a~%" e))))

(defun start-monitor ()
  "Start the interactive VM monitor"
  (format t "~%bard VM Monitor - Type (help) for commands~%")
  
  (loop
   (let ((cmd (read-monitor-command)))
     (when (eq (eval-monitor-command cmd) :quit)
       (return))))
  
  ;; Cleanup: stop all running VMs
  
  (format t "Bye!~%"))

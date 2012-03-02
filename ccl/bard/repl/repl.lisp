
(in-package :bard)

(defun repl ()
  (format t "~%Bard 0.1 repl test~%")
  (block repl
    (let ((prompt "> "))
      (loop
         (format t "~%~A" prompt)
         (let* ((line (cl:read-line))
                (cmd (cl:read-from-string line)))
           (case cmd
             ((:quit :q)(return-from repl (ccl:quit)))
             (t (progn
                  (terpri)
                  (print (read line) t)
                  (terpri)))))))))

;;; (repl)
;;; (ccl:save-application "/Volumes/ymra/Users/mikel/Desktop/bard" :toplevel-function #'repl :prepend-kernel t)
;;; -*- Mode: Lisp -*-

(in-package "CL.EXT.DACF.UNIFICATION") ; DACF = Data And Control Flow.


(defun make-var-name (&optional (s (gensym "UNIFVAR-")) (package *package*))
  (intern (concatenate 'string "?" (symbol-name s)) package))


(defun variablep (x)
  (and (symbolp x)
       (or (char= (char (symbol-name x) 0) #\?)
           (string= x "_"))))

(defun variable-any-p (x)
  (and (symbolp x)
       (or (string= x "_")
           (string= x "?_"))))


;;; end of file -- variables.lisp --

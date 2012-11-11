;;;; bard.lisp

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(defmethod plist->alist ((p null)) nil)

(defmethod plist->alist ((p cons)) 
  (assert (evenp (length p))() "Malformed property list: ~s" p)
  (loop for (k v . rest) on p by #'cddr
       collect (cons k v)))

;;; ---------------------------------------------------------------------
;;; globals
;;; ---------------------------------------------------------------------

(defparameter *bard-environment* nil)
(defparameter *bard-standard-output* cl:*standard-output*)

;;; ---------------------------------------------------------------------
;;; reader
;;; ---------------------------------------------------------------------

(defun bard-name->character (nm)
  (name-char nm))

(defun bard-character->name (nm)
  (char-name nm))

(defun standard-lisp-readtable ()
  ccl::%standard-readtable%)

(defun read-square-bracket (stream char)
  (let ((expr (cl:read-delimited-list #\] stream nil)))
    (cons (intern "list" :bard) expr)))

(defun read-curly-bracket (stream char)
  (let ((expr (cl:read-delimited-list #\} stream nil)))
    (cons (intern "table" :bard) expr)))

(defun bard-character-delimiter? (ch)
  (or (not (standard-char-p ch))
      (member ch '(#\" #\' #\( #\) #\` #\: #\; #\# #\[ #\] #\{ #\} #\| #\, #\space #\tab #\newline) :test #'char=)))

(defun bard-single-character-input? (ch0 ch1)
  (or (null ch1)
      (bard-character-delimiter? ch1)))

(defun bard-unicode-character-input? (ch0 ch1)
  (and (or (char= ch0 #\u)
           (char= ch0 #\U))
       (char= ch1 #\+)))

(defun bard-named-character-input? (ch0 ch1)
  (not (bard-character-delimiter? ch1)))

(defmethod bard-unicode-code->character ((code integer))
  (code-char code))

(defmethod bard-unicode-code->character ((code string))
  (bard-unicode-code->character (parse-integer code :radix 16)))

(defun bard-read-unicode-character (stream)
  (let ((chars '()))
    (block reading
      (loop for ch = (read-char stream nil nil nil) then (read-char stream nil nil nil)
         do (if (or (null ch)
                    (bard-character-delimiter? ch))
                (return-from reading
                  (bard-unicode-code->character (coerce (reverse chars) 'string)))
                (push ch chars))))))

(defun bard-read-named-character (chars stream)
  (let ((chars (reverse chars)))
    (block reading
      (loop for ch = (read-char stream nil nil nil) then (read-char stream nil nil nil)
         do (if (or (null ch)
                    (bard-character-delimiter? ch))
                (return-from reading
                  (bard-name->character (coerce (reverse chars) 'string)))
                (push ch chars))))))

(defun read-bard-character (stream char)
  (let* ((ch0 (read-char stream))
         (ch1 (peek-char nil stream nil nil nil)))
    (cond
      ((bard-single-character-input? ch0 ch1) ch0)
      ((bard-unicode-character-input? ch0 ch1)(bard-read-unicode-character stream))
      ((bard-named-character-input? ch0 ch1)(bard-read-named-character (list ch0) stream))
      (t (error "Invalid character syntax")))))

(defparameter +bard-readtable+
  (let ((brt (copy-readtable)))
    (setf (readtable-case brt) :preserve)
    (set-macro-character #\[ #'read-square-bracket nil brt)
    (set-macro-character #\] (get-macro-character #\) nil) nil brt)
    (set-macro-character #\{ #'read-curly-bracket nil brt)
    (set-macro-character #\} (get-macro-character #\) nil) nil brt)
    (set-macro-character #\\ #'read-bard-character nil brt)
    brt))

(defmethod bard-read ((in stream))
  (let ((*readtable* +bard-readtable+)
        (*package* (find-package :bard)))
    (cl:read in nil nil nil)))

(defmethod bard-read ((instr string))
  (with-input-from-string (in instr)
    (bard-read in)))

;;; ---------------------------------------------------------------------
;;; bard values
;;; ---------------------------------------------------------------------

(defun nothing ()
  nil)

(define-symbol-macro bard::|nothing| (nothing))

(defun undefined ()
  (intern "undefined" :bard))

(define-symbol-macro bard::|undefined| (undefined))

(defun true ()
  (intern "true" :bard))

(define-symbol-macro bard::|true| (true))

(defun false ()
  (intern "false" :bard))

(define-symbol-macro bard::|false| (false))

;;; ---------------------------------------------------------------------
;;; toplevel environment
;;; ---------------------------------------------------------------------

(defmethod def-bard-var ((vname string) val)
  (set (intern vname :bard) val))

(defun init-bard-globals ()
  (def-bard-var "list" #'(lambda (&rest elts) (fset:convert 'fset:seq elts)))
  (def-bard-var "table" #'(lambda (&rest elts) (fset:convert 'fset:map (plist->alist elts)))))

(defun gref (vname)
  (symbol-value vname))

;;; ---------------------------------------------------------------------
;;; evaluation
;;; ---------------------------------------------------------------------

;;; special forms

(defun bard-special-form? (expr) nil)

(defun bard-compile-special-form (expr env)
  )

;;; macros

(defun bard-macro-form? (expr) nil)

(defun bard-macroexpand (expr)
  )

;;; apply

(defmethod bard-apply ((op function) (args null))
  (cl:apply op args))

(defmethod bard-apply ((op function) (args cons))
  (cl:apply op args))

;;; compiling funcalls

(defun bard-compile-funcall (expr env)
  (let* ((opform (car expr))
         (argforms (cdr expr))
         (op (bard-eval opform env))
         (args (mapcar (lambda (a)(bard-eval a env)) argforms)))
    `(bard-apply ,op (cl:list ,@args))))

;;; expression compiler

(defun bard-compile-app (expr env)
  (cond
    ((bard-special-form? expr)(bard-compile-special-form expr env))
    ((bard-macro-form? expr)(bard-compile (bard-macroexpand expr) env))
    (t (bard-compile-funcall expr env))))

(defun bard-compile-variable-reference (expr env)
  `(ccl::cheap-eval-in-environment ,expr ,env))

(defmethod bard-compile ((expr null) &optional (env nil)) expr)
(defmethod bard-compile ((expr (eql 'bard::|nothing|)) &optional (env nil)) nil)
(defmethod bard-compile ((expr (eql 'bard::|undefined|)) &optional (env nil)) expr)
(defmethod bard-compile ((expr (eql 'bard::|true|)) &optional (env nil)) expr)
(defmethod bard-compile ((expr (eql 'bard::|false|)) &optional (env nil)) expr)
(defmethod bard-compile ((expr number) &optional (env nil)) expr)
(defmethod bard-compile ((expr character) &optional (env nil)) expr)
(defmethod bard-compile ((expr keyword) &optional (env nil)) expr)
(defmethod bard-compile ((expr string) &optional (env nil)) expr)
(defmethod bard-compile ((expr fset:seq) &optional (env nil)) expr)
(defmethod bard-compile ((expr fset:map) &optional (env nil)) expr)

(defmethod bard-compile ((expr symbol) &optional (env nil)) 
  (bard-compile-variable-reference expr env))

(defmethod bard-compile ((expr cons) &optional (env nil)) 
  (bard-compile-app expr env))

;;; toplevel eval

(defun bard-eval (expr &optional (env nil))
  (apply #'values (ccl::toplevel-eval (bard-compile expr env) env)))

;;; ---------------------------------------------------------------------
;;; printer
;;; ---------------------------------------------------------------------

(defmethod as-string (val)
  (format nil "~S" val))

(defmethod as-string ((val (eql '())))
  "nothing")

(defmethod as-string ((val (eql (undefined))))
  "undefined")

(defmethod as-string ((val (eql (true))))
  "true")

(defmethod as-string ((val (eql (false))))
  "false")

(defmethod as-string ((val character))
  (let ((nm (bard-character->name val)))
    (if nm
        (concatenate 'string "\\" nm)
        (if (standard-char-p val)
            (concatenate 'string "\\" (string val))
            (format nil "\\~4,'0x" (char-code val))))))

(defmethod as-string ((val keyword))
  (concatenate 'string ":" (symbol-name val)))

(defmethod as-string ((val string))
  (concatenate 'string "\"" val "\""))

(defun bard-print (val &optional (out *standard-output*))
  (format out "~A" (as-string val)))

;;; ---------------------------------------------------------------------
;;; driver and repl
;;; ---------------------------------------------------------------------

(defmethod bard ((in stream))
  (let ((*readtable* +bard-readtable+))
    (bard-print (bard-eval (bard-read in) *bard-environment*) *bard-standard-output*)
    (terpri *bard-standard-output*)
    (force-output *bard-standard-output*)))

(defmethod bard ((instr string))
  (with-input-from-string (in instr)
    (bard in)))

#| tests

(init-bard-globals)
(bard "undefined")
(bard "nothing")
(bard "true")
(bard "false")
(bard "0")
(bard "1.2")
(bard "1.2e3")
(bard "\\A")
(bard "\\tab")
(bard "\\u+03bb")
(bard "list")
(bard "|Foo bar|")
(bard ":Foo")
(bard "\"\"")
(bard "()")
(bard "[]")
(bard "{}")
(bard "\"Foo bar baz\"")
(bard "(list)")
(bard "(list 1 2 3)")
(bard "[:Foo :bar [:baz]]")
(bard "{:Foo :bar :baz {:grault :quux}}")
(bard "{:Foo :bar :baz {:grault [:quux :wibble]}}")


|#

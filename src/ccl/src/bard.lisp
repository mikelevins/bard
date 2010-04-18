;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       bard runtime 
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :|bard-internal|)

;;; ============================================================
;;; singletons
;;; ============================================================
;;; from Tim Bradshaw's example at:
;;; http://www.tfeb.org/programs/lisp/singleton-class.lisp
;;; copyright 2002 by TIm Bradshaw

(defclass singleton-class (standard-class)
  ((instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)
                                (superclass standard-class))
  ;; it's OK for a standard class to be a superclass of a singleton
  ;; class
  t)

(defmethod validate-superclass ((class singleton-class)
                                (superclass singleton-class))
  ;; it's OK for a singleton class to be a subclass of a singleton class
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass singleton-class))
  ;; but it is not OK for a standard class which is not a singleton class
  ;; to be a subclass of a singleton class
  nil)

(defmethod make-instance ((class singleton-class)
                          &key)
  (with-slots (instance) class
    (or instance
        (setf instance (call-next-method)))))

(defvar *singleton-classes* '())

(defmethod initialize-instance :after ((c singleton-class) &key)
  (pushnew c *singleton-classes*))

(defun reset-singleton-classes ()
  ;; This means you will get new singletons from now on.
  (loop for c in *singleton-classes*
        do (setf (slot-value c 'instance) nil)))

;;; ============================================================
;;; general operations on values
;;; ============================================================

(defmethod = (x y) (cl:eql x y))

;;; ============================================================
;;; basic Bard types
;;; ============================================================

;;; ------------------------------------------------------------
;;; Nothing
;;; ------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))

(defmethod print-value ((n nothing)(str stream))
  (declare (ignore n))
  (format str "nothing"))

(defun nothing ()(make-instance 'nothing))

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x nothing))(declare (ignore x)) t)
(defun something? (x)(not (nothing? x)))

(defmethod = ((x nothing) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y nothing))
  (declare (ignore x))
  nil)

(defmethod = ((x nothing) (y nothing))
  (declare (ignore x y))
  t)

(defmethod fset:compare ((a nothing) (b nothing))
  ':equal)

(fset:define-cross-type-compare-methods nothing)

;;; ------------------------------------------------------------
;;; Number
;;; ------------------------------------------------------------

(defmethod number? (x) (cl:numberp x))

(defmethod print-value ((n cl:number)(str stream))
  (format str "~A" n))

(defmethod = ((x cl:number) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:number))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:number) (y cl:number))
  (cl:= x y))

;;; ------------------------------------------------------------
;;; Character
;;; ------------------------------------------------------------

(defmethod print-value ((c cl:character)(str stream))
  (format str "\\")
  (write-char c str))

(defmethod character? (x)(declare (ignore x)) nil)
(defmethod character? ((x cl:character))(declare (ignore x)) t)

(defmethod = ((x cl:character) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:character))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:character) (y cl:character))
  (cl:char= x y))

;;; Keyword
;;; ------------------------------------------------------------

(defmethod print-value ((k cl:keyword)(str stream))
  (format str "~A:" (symbol-name k)))

(defmethod keyword ((s cl:string))
  (cl:intern s (find-package :keyword)))

(defmethod keyword? (x)(declare (ignore x)) nil)
(defmethod keyword? ((x cl:keyword))(declare (ignore x)) t)

(defmethod = ((x cl:keyword) y)
  (declare (ignore y))
  nil)

;;; Symbol
;;; ------------------------------------------------------------

(defmethod print-value ((s cl:symbol)(str stream))
  (format str "~A" (symbol-name s)))

(defmethod self-evaluating? ((x cl:symbol))
  (declare (ignore x))
  nil)

(defmethod symbol ((s cl:symbol)) s)

(defmethod symbol ((s cl:string))
  (cl:intern s (find-package :|bard-internal|)))

(defmethod symbol? (x)(declare (ignore x)) nil)
(defmethod symbol? ((x cl:symbol))(declare (ignore x)) t)

;;; ------------------------------------------------------------
;;; Booleans
;;; ------------------------------------------------------------

(defun boolean? (x)
  (or (eql x t)
      (eql x nil)))

;;; True
;;; ------------------------------------------------------------

(defmethod print-value ((v (eql t))(str stream))
  (format str "true"))

(defun true () t)
(defun true? (x) (eql x t))

;;; False
;;; ------------------------------------------------------------

(defmethod print-value ((v (eql nil))(str stream))
  (format str "false"))

(defun false () nil)
(defun false? (x) (eql x nil))

;;; ------------------------------------------------------------
;;; Sequences
;;; ------------------------------------------------------------

(defmethod print-value ((s cl:cons)(str stream))
  (progn
    (format str "(")
    (let* ((len (length s)))
      (dotimes (i len)
        (when (< 0 i len) (format str " "))
        (print-value (elt s i) str)))
    (format str ")")))

(defmethod sequence? (x)(declare (ignore x)) nil)
(defmethod sequence? ((x cl:cons))(declare (ignore x)) t)

(defmethod self-evaluating? ((x cl:cons))
  (declare (ignore x))
  nil)

(defun prepend (item seq)
  (cons item seq))

(defun |sequence| (&rest items)
  (cl:apply #'cl:list items))

(defmethod = ((x cl:cons) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:cons))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:cons) (y cl:cons))
  (every (lambda (i j) (= i j)) 
               x y))

;;; ------------------------------------------------------------
;;; Text
;;; ------------------------------------------------------------

(defmethod text? (x)(declare (ignore x)) nil)
(defmethod text? ((x cl:string))
  (declare (ignore x))
  t)

(defmethod text ((s cl:string)) s)

;;; ------------------------------------------------------------
;;; Maps
;;; ------------------------------------------------------------

(defmethod print-value ((m fset:map)(stream stream))
  (progn
    (format stream "{ ")
    (let ((i 0))
      (fset:do-map (k v m)
        (when (> i 0)
          (format stream " "))
        (print-value k stream)
        (format stream " ")
        (print-value v stream)
        (incf i))
      (when (> i 0)
        (format stream " ")))
    (format stream "}")))

(defmethod map? (x)(declare (ignore x)) nil)
(defmethod map? ((x fset:map))(declare (ignore x)) t)

(defun map (&rest entries)
  (if (null entries)
    (fset:empty-map)
    (fset:map-union (fset:with (fset:empty-map)
                          (cl:first entries)(cl:second entries))
               (cl:apply 'map (nthcdr 2 entries)))))

(defmethod get-key ((m fset:map) key &optional (default (nothing)))
  (fset:lookup (fset:with-default m default) key))

(defmethod = ((x fset:map) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y fset:map))
  (declare (ignore x))
  nil)

(defmethod = ((x fset:map) (y fset:map))
  (and (eql (fset:compare x y)
            :equal)
       t))

;;; ============================================================
;;; Protocols and Categories
;;; ============================================================

;;; ============================================================
;;; Functions and Methods
;;; ============================================================

;;; ============================================================
;;; Bard Libraries
;;; ============================================================

;;; ============================================================
;;; Bard Modules
;;; ============================================================

(in-package :|bard-internal|)

(defun init-modules ()
  )

;;; ============================================================
;;; Bard Reader
;;; ============================================================

(in-package :|bard-internal|)

;;; ----------------------------------------------------------------------
;;; eof-value
;;; ----------------------------------------------------------------------

(defclass eof ()())
(defparameter $eof (make-instance 'eof))
(defmethod eof? (x)(declare (ignore x)) nil)
(defmethod eof? ((e eof))(declare (ignore e)) t)

(defclass end-of-sequence ()())
(defparameter $end-of-sequence (make-instance 'end-of-sequence))
(defmethod end-of-sequence? (x)(declare (ignore x)) nil)
(defmethod end-of-sequence? ((e end-of-sequence))(declare (ignore e)) t)

(defclass end-of-map ()())
(defparameter $end-of-map (make-instance 'end-of-map))
(defmethod end-of-map? (x)(declare (ignore x)) nil)
(defmethod end-of-map? ((e end-of-map))(declare (ignore e)) t)

;;; ----------------------------------------------------------------------
;;; Bard characters
;;; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *character-name-table* (make-hash-table :test 'equalp)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod define-character-name ((cid string)(ch cl:character))
    (setf (gethash cid *character-name-table*) ch)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod find-character ((cid string))
    (gethash cid *character-name-table* nil)))
  
  ;; set up some symbolic character names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (define-character-name "space" #\space)
    (define-character-name "tab" #\tab)
    (define-character-name "return" #\return)
    (define-character-name "newline" #\newline)))

;;; ----------------------------------------------------------------------
;;; conversion from lisp objects created by the reader to bard objects
;;; ----------------------------------------------------------------------

(defmethod reader-object->bard-expression (thing)
  (error "Bard reader: Don't know how to convert ~S to a bard expression"
         thing))

;;; special reader values
(defmethod reader-object->bard-expression ((x (eql nil))) (nothing))
(defmethod reader-object->bard-expression ((x (eql 'cl:nil))) (nothing))

(defmethod reader-object->bard-expression ((x end-of-map)) x)

(defmethod reader-object->bard-expression ((x end-of-sequence)) x)

;;; bard expressions

(defmethod reader-object->bard-expression ((s cl:string)) 
  (text s))

(defmethod reader-object->bard-expression ((n cl:number)) n)

(defmethod reader-object->bard-expression ((c cl:character)) 
  (character c))

;;; the Bard reader uses leading and trailing colons to recognize
;;; keywords, and the printer adds a colon to visually distinguish a keyword,
;;; but the leading and trailing colons are not part of the keyword's name
(defun %strip-keyword-colons (sname)
  (let* ((slen (length sname))
         (start (if (eql #\: (elt sname 0))
                    1
                    0))
         (end (if (eql #\: (elt sname (1- slen)))
                  (1- slen)
                  slen)))
    (subseq sname start end)))

(defmethod reader-object->bard-expression ((s cl:symbol)) 
  (let* ((sname (symbol-name s))
         (slen (length sname)))
    (cond
      ((eql #\: (elt sname 0)) (keyword (%strip-keyword-colons sname)))
      ((eql #\: (elt sname (1- slen))) (keyword (%strip-keyword-colons sname)))
      ((equal "true" (symbol-name s)) (true))
      ((equal "false" (symbol-name s)) (false))
      ((equal "nothing" (symbol-name s)) (nothing))
      (t (symbol sname)))))

;;; (...) => sequence
(defmethod reader-object->bard-expression ((c cl:list)) 
  (if (listp (cdr c))
      (cl:apply '|sequence| c)
      c))

(defmethod reader-object->bard-expression ((c fset:seq)) 
  c)

(defmethod reader-object->bard-expression ((c fset:map)) 
  c)


;;; ----------------------------------------------------------------------
;;; the bard reader
;;; ----------------------------------------------------------------------
;;; the reader converts an input string to a list of symbolic tokens
;;; the tokens are either numbers, strings, or symbols

;;; set up the bard read table

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +initial-read-table+ *readtable*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +bard-read-table+ (copy-readtable *readtable*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case +bard-read-table+) :preserve))

;; the character reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\\ 
                       (lambda (stream char)
                         (let* ((char-sym (|bard-internal|::read stream))
                                (digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                           (cond
                             ((eof? char-sym) (error "Unexpected end of input while reading a character"))
                             ((and (typep char-sym 'cl:number)
                                   (<= 0 char-sym 9))
                              (elt digits char-sym))
                             ((typep char-sym 'cl:symbol)
                              (let ((char-name (cl:symbol-name char-sym)))
                                (if (= 1 (length char-name))
                                    (character (elt char-name 0))
                                    (or (find-character char-name)
                                        (error "Invalid character syntax: ~x~S" char char-sym)))))
                             (t (error "Invalid character syntax: ~S" char-sym)))))
                       nil +bard-read-table+))

;; the sequence reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\(
                       (lambda (stream char)
                         (declare (ignore char))
                         (let ((elements '()))
                           (block reading
                             (loop
                                (let ((next-elt (|bard-internal|::read stream)))
                                  (cond
                                    ((eof? next-elt)(error "Unexpected end of input while reading a sequence"))
                                    ((end-of-sequence? next-elt) 
                                     (return-from reading  (cl:apply '|sequence| (reverse elements))))
                                    (t (setf elements (cons next-elt elements)))))))))
                       nil +bard-read-table+))

;;; end-of-sequence
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\)
                       (lambda (stream char)
                         (declare (ignore stream char))
                         $end-of-sequence)
                       nil +bard-read-table+))

;; the sequence reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\[
                       (lambda (stream char)
                         (declare (ignore char))
                         (let ((sequence-op (symbol "sequence"))
                               (elements '()))
                           (block reading
                             (loop
                                (let ((next-elt (|bard-internal|::read stream)))
                                  (cond
                                    ((eof? next-elt)(error "Unexpected end of input while reading a sequence"))
                                    ((end-of-sequence? next-elt) (return-from reading 
                                                                   (prepend
                                                                    sequence-op
                                                                    (cl:apply '|sequence| (reverse elements)))))
                                    (t (progn
                                         (setf elements (cons next-elt elements))))))))))
                       nil +bard-read-table+))

;;; end-of-sequence
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\]
                       (lambda (stream char)
                         (declare (ignore stream char))
                         $end-of-sequence)
                       nil +bard-read-table+))

;; the map reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\{
                       (lambda (stream char)
                         (declare (ignore char))
                         (let ((elements '()))
                           (block reading
                             (loop
                                (let ((next-elt (|bard-internal|::read stream)))
                                  (cond
                                    ((eof? next-elt)(error "Unexpected end of input while reading a map"))
                                    ((end-of-map? next-elt) (return-from reading 
                                                              (cl:apply 'map (reverse elements))))
                                    (t (progn
                                         (setf elements (cons next-elt elements))))))))))
                       nil +bard-read-table+))

;;; end-of-map
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\}
                       (lambda (stream char)
                         (declare (ignore stream char))
                         $end-of-map)
                       nil +bard-read-table+))

(in-package "CCL")

;;; ----------------------------------------------------------------------
;;; reader surgery
;;; ----------------------------------------------------------------------
;;; some customizations to make the Bard reader possible

;;; from the CCL sources. Redefined here because the CCL reader hardcodes
;;; its treatment of colons, making Bard's treatment of colons impossible.
;;; This redefinition keeps everything the same except that, if *readtable*
;;; is bound to the bard readtable, then colons are not treated specially.

(eval-when (:compile-toplevel :execute)
(def-accessors %svref
  token.string
  token.ipos
  token.opos
  token.len)

(defmacro with-token-buffer ((name) &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    `(let* ((,name (vector (%get-token-string 16) 0 0 16 nil)))
       (declare (dynamic-extent ,name))
       (unwind-protect
         (locally ,@decls ,@body)
         (%return-token-string ,name)))))
)

(let ((*warn-if-redefine-kernel* nil))
  (defun %collect-xtoken (token stream 1stchar)
    (let* ((escapes ())
           (nondots nil)
           (explicit-package *read-suppress*)
           (double-colon t)
           (multi-escaped nil))
      (do* ((attrtab (rdtab.ttab *readtable*))
            (char 1stchar (read-char stream nil :eof )))
           ((eq char :eof))
        (flet ((add-note-escape-pos (char token escapes)
                 (push (token.opos token) escapes)
                 (%add-char-to-token char token)
                 escapes))
          (let* ((attr (%character-attribute char attrtab)))
            (declare (fixnum attr))
            (when (or (= attr $cht_tmac)
                      (= attr $cht_wsp))
              (when (or (not (= attr $cht_wsp)) %keep-whitespace%)
                (unread-char char stream))
              (return ))
            (if (= attr $cht_ill)
                (signal-reader-error stream "Illegal character ~S." char)
                (if (= attr $cht_sesc)
                    (setq nondots t 
                          escapes (add-note-escape-pos (%read-char-no-eof stream) token escapes))
                    (if (= attr $cht_mesc)
                        (progn 
                          (setq nondots t)
                          (loop
                             (multiple-value-bind (nextchar nextattr) (%next-char-and-attr-no-eof stream attrtab)
                               (declare (fixnum nextattr))
                               (if (= nextattr $cht_mesc) 
                                   (return (setq multi-escaped t))
                                   (if (= nextattr $cht_sesc)
                                       (setq escapes (add-note-escape-pos (%read-char-no-eof stream) token escapes))
                                       (setq escapes (add-note-escape-pos nextchar token escapes)))))))
                        (let* ((opos (token.opos token)))         ; Add char to token, note 1st colonpos
                          (declare (fixnum opos))
                          (if (and (not (eql *readtable* |bard-internal|::+bard-read-table+)) ; special case for the Bard readtable 
                                   (eq char #\:)       ; (package-delimiter-p char ?)
                                   (not explicit-package))
                              (let* ((nextch (%read-char-no-eof stream)))
                                (if (eq nextch #\:)
                                    (setq double-colon t)
                                    (progn
                                      (unread-char nextch stream)
                                      (setq double-colon nil)))
                                (%casify-token token escapes)
                                (setq explicit-package (%token-package token opos nondots stream)
                                      nondots t
                                      escapes nil)
                                (setf (token.opos token) 0))
                              (progn
                                (unless (eq char #\.) (setq nondots t))
                                (%add-char-to-token char token))))))))))
      (values (or escapes multi-escaped) (if *read-suppress* nil explicit-package) nondots double-colon))))

;;; ----------------------------------------------------------------------
;;; Bard's read function
;;; ----------------------------------------------------------------------

(in-package :|bard-internal|)

(defmethod read ((in stream))
  (let ((*readtable* |bard-internal|::+bard-read-table+)
        (*package* (find-package :|bard-internal|)))
	(let ((obj (cl:read in nil |bard-internal|::$eof nil)))
      (|bard-internal|::reader-object->bard-expression obj))))

(defmethod read ((in string))
  (with-input-from-string (s in)
	(read s)))

(defun read-input (input)
  (let ((source-map (make-hash-table :test #'eq :shared nil))
        (*readtable* |bard-internal|::+bard-read-table+)
        (*package* (find-package :|bard|)))
    (with-input-from-string (input-stream input)
      (multiple-value-bind (input-form env print-result)
          (ccl::read-toplevel-form input-stream :eof-value |bard-internal|::$eof :map source-map)
        (values input-form env)))))

;;; ============================================================
;;; Bard Printer
;;; ============================================================

(in-package :|bard-internal|)

(defmethod print-value (val stream)
  (cl:print-object val stream))

;;; ============================================================
;;; Bard Toplevel
;;; ============================================================

(in-package :|bard-internal|)

(defparameter |bard|::|*module*| (find-package :|bard|))

(defun |quit| ()
  (throw 'quitting
    (progn
      (format *standard-output* "~%bard terminated" )
      (values))))

;;; when we read a map, we need to eval the keys and values
(defun eval-input-map (imap env)
  (let ((result (fset:empty-map)))
    (fset:do-map (k v imap)
      (let* ((kvals (ccl::toplevel-eval k env))
             (k (if (null kvals)
                    (nothing)
                    (cl:first kvals)))
             (vvals (ccl::toplevel-eval v env))
             (v (if (null vvals)
                    (nothing)
                    (cl:first vvals))))
        (if (something? v)
            (if (something? k)
                (setf (fset:@ result k) v)
                (error "Attempt to associate nothing with a value in a map")))))
    result))

(defun repl ()
  (init-modules)
  (let ((*package* (find-package :|bard|)))
    (catch 'quitting 
      (loop
         (format *standard-output* "~%bard> ")
         (let ((input-string (read-line *standard-input* nil nil nil)))
           (multiple-value-bind (input-form env)
               (|bard-internal|::read-input input-string)
             (let* ((input-form (if (map? input-form)
                                        (eval-input-map input-form env)
                                        input-form))
                    (values (ccl::toplevel-eval input-form env)))
               (dolist (v values)
                 (format *standard-output* "~%")
                 (|bard-internal|::print-value v *standard-output*)))))))))

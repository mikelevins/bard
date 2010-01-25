;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       bard
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

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

(defmethod reader-object->bard-expression ((e expression)) e)

(defmethod reader-object->bard-expression (thing)
  (error "Bard reader: Don't know how to convert ~S to a bard expression"
         thing))

;;; special reader values
(defmethod reader-object->bard-expression ((x end-of-map)) x)

(defmethod reader-object->bard-expression ((x end-of-sequence)) x)

;;; bard expressions

(defmethod reader-object->bard-expression ((s cl:string)) 
  (make-instance 'text-expression :value s))

(defmethod reader-object->bard-expression ((n cl:number)) 
  (make-instance 'number-expression :value n))

(defmethod reader-object->bard-expression ((c cl:character)) 
  (make-instance 'character-expression :value c))

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
      ((eql #\: (elt sname 0)) (make-instance 'keyword-expression :value (%strip-keyword-colons sname)))
      ((eql #\: (elt sname (1- slen))) (make-instance 'keyword-expression :value (%strip-keyword-colons sname)))
      ((equal "true" (symbol-name s)) (make-instance 'true-expression))
      ((equal "false" (symbol-name s)) (make-instance 'false-expression))
      ((equal "void" (symbol-name s)) (make-instance 'void-expression))
      (t (make-instance 'symbol-expression :value sname)))))

;;; (...) => sequence
(defmethod reader-object->bard-expression ((c cl:list)) 
  (apply 'sequence-expression c))

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
                         (let* ((char-sym (bard::read stream))
                                (digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                           (cond
                             ((eof? char-sym) (error "Unexpected end of input while reading a character"))
                             ((and (typep char-sym 'number-expression)
                                   (<= 0 (value char-sym) 9))
                              (elt digits (value char-sym)))
                             ((typep char-sym 'symbol-expression)
                              (let ((char-name (value char-sym)))
                                (if (= 1 (length char-name))
                                    (elt char-name 0)
                                    (or (find-character (value char-sym))
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
                                (let ((next-elt (bard::read stream)))
                                  (cond
                                    ((eof? next-elt)(error "Unexpected end of input while reading a sequence"))
                                    ((end-of-sequence? next-elt) (return-from reading 
                                                                   (apply 'sequence-expression (reverse elements))))
                                    (t (progn
                                         (setf elements (cons next-elt elements))))))))))
                       nil +bard-read-table+))

;;; end-of-sequence
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\)
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
                                (let ((next-elt (bard::read stream)))
                                  (cond
                                    ((eof? next-elt)(error "Unexpected end of input while reading a map"))
                                    ((end-of-map? next-elt) (return-from reading 
                                                              (apply 'map-expression (reverse elements))))
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
                          (if (and (not (eql *readtable* bint::+bard-read-table+)) ; special case for the Bard readtable 
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



(in-package :bard)

;;; ----------------------------------------------------------------------
;;; Bard's read function
;;; ----------------------------------------------------------------------

(defmethod read ((in stream))
  (let ((*readtable* bint::+bard-read-table+)
		(*package* (find-package :bard)))
	(let ((obj (cl:read in nil bint::$eof nil)))
      (bint::reader-object->bard-expression obj))))

(defmethod read ((in string))
  (with-input-from-string (s in)
	(read s)))

;;; ----------------------------------------------------------------------
;;; testing
;;; ----------------------------------------------------------------------

(defun read-test (s)
  (let ((x (read s)))
    (format t "~%~a" x)))

(defparameter $test-expressions
  '(
    ;; text
    "\"foo bar\""
    ;; numbers
    "123.45" "1" "#b101"
    ;; characters
    "\\c" "\\1" "\\space"
    ;; boolean and void
    "true" "false" "void"
    ;; symbols
    "foo" "bard.lang/foo"
    ;; keywords
    ":foo" "foo:" ":foo:"
    ;; sequences
    "()" "(foo)" "(+ (- 5 2)(- 4 3))"
    ;; maps
    "{}" "{name: foo}" "{first: {a b} second: {c d}}"
    ))

(defun run-read-test ()
  (terpri)
  (dolist (s $test-expressions)
    (read-test s))
  (terpri))
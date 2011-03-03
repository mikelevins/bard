;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

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
;;; building bard values from the reader
;;; ----------------------------------------------------------------------

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
      ((equal "nothing" (symbol-name s)) (nothing))
      ((equal "true" (symbol-name s)) (true))
      ((equal "false" (symbol-name s)) (false))
      (t sname))))

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

#|
;; the character reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\\ 
                       (lambda (stream char)
                         (let* ((char-sym (|bard-internal|::read stream))
                                (digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                           (cond
                             ((eof? char-sym) (error "Unexpected end of input while reading a character"))
                             ((and (typep char-sym 'number)
                                   (<= 0 (value char-sym) 9))
                              (elt digits (value char-sym)))
                             ((typep char-sym 'symbol)
                              (let ((char-name (name char-sym)))
                                (if (= 1 (length (cl:symbol-name char-name)))
                                    (character (elt (cl:symbol-name char-name) 0))
                                    (or (find-character (cl:symbol-name (name char-sym)))
                                        (error "Invalid character syntax: ~x~S" char char-sym)))))
                             (t (error "Invalid character syntax: ~S" char-sym)))))
                       nil +bard-read-table+))

;; the sequence reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\(
                       (lambda (stream char)
                         (declare (ignore char))
                         (let ((elements '())
                               (pending-pair nil))
                           (block reading
                             (loop
                                (let ((next-elt (|bard-internal|::read stream)))
                                  (cond
                                    ((eof? next-elt)(error "Unexpected end of input while reading a sequence"))
                                    ((end-of-sequence? next-elt) 
                                     (if pending-pair
                                         (error "Unexpected end of input while reading a pair")
                                         (return-from reading  (if (and (= 1 (length elements))
                                                                        (consp (cl:first elements)))
                                                                   (cl:first elements)
                                                                   (cl:apply 'sequence (reverse elements))))))
                                    ((comma? next-elt) 
                                     (if pending-pair
                                         (error "Syntx error: duplicate commas")
                                         (progn
                                           (setf pending-pair (car elements))
                                           (setf elements (cdr elements)))))
                                    (t (if pending-pair
                                           (progn
                                             (setf elements (cons (pair pending-pair next-elt) elements))
                                             (setf pending-pair nil))
                                           (setf elements (cons next-elt elements))))))))))
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
                                                                    (cl:apply 'sequence (reverse elements)))))
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
                                    ((comma? next-elt) nil)
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
|#
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



(in-package :|bard-internal|)

;;; ----------------------------------------------------------------------
;;; Bard's read function
;;; ----------------------------------------------------------------------

(defmethod read ((in stream))
  (let ((*readtable* |bard-internal|::+bard-read-table+)
		(*package* (find-package :|bard|)))
	(let ((obj (cl:read in nil |bard-internal|::$eof nil)))
      (|bard-internal|::reader-object->bard-expression obj))))

(defmethod read ((in string))
  (with-input-from-string (s in)
	(read s)))

#|

(|bard-internal|:read "nothing")
(|bard-internal|:read "10")
(|bard-internal|:read "12.34")
(|bard-internal|:read "\\space")
(|bard-internal|:read "\\A")
(|bard-internal|:read ":Foo")
(|bard-internal|:read "bar:")
(|bard-internal|:read "baZZ")
(|bard-internal|:read "true")
(|bard-internal|:read "false")
(|bard-internal|:read "()")
(|bard-internal|:read "[]")
(|bard-internal|:read "(0 1 2 3 4 5)")
(|bard-internal|:read "[0 1 2 3 4 5]")
(|bard-internal|:read "\"\"")
(|bard-internal|:read "\"Hello!\"")
(|bard-internal|:read "{ }")
(|bard-internal|:read "{ greeting \"Hello!\"}")




|#
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader01.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard reader pass 1: convert text to abstract syntax
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defclass end-of-file ()())
(defun end-of-file ()(make-instance 'end-of-file))
(defmethod end-of-file? (x) nil)
(defmethod end-of-file? ((x end-of-file)) t)

(defclass end-of-sequence ()())
(defun end-of-sequence ()(make-instance 'end-of-sequence))
(defmethod end-of-sequence? (x) nil)
(defmethod end-of-sequence? ((x end-of-sequence)) t)

(defclass end-of-map ()())
(defun end-of-map ()(make-instance 'end-of-map))
(defmethod end-of-map? (x) nil)
(defmethod end-of-map? ((x end-of-map)) t)

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
                         (declare (ignore char))
                         (let ((item (bard::read stream)))
                           `(:character ,item)))
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
                                    ((end-of-file? next-elt)(error "Unexpected end of input while reading a sequence"))
                                    ((end-of-sequence? next-elt) 
                                     (return-from reading  (cons :sequence (reverse elements))))
                                    (t (setf elements (cons next-elt elements)))))))))
                       nil +bard-read-table+))

;;; end-of-sequence
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\)
                       (lambda (stream char)
                         (declare (ignore stream char))
                         (end-of-sequence))
                       nil +bard-read-table+))

;; the sequence reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\[
                       (lambda (stream char)
                         (declare (ignore char))
                         (let ((elements '()))
                           (block reading
                             (loop
                                (let ((next-elt (bard::read stream)))
                                  (cond
                                    ((end-of-file? next-elt)(error "Unexpected end of input while reading a sequence"))
                                    ((end-of-sequence? next-elt) 
                                     (return-from reading  (cons :sequence-literal (reverse elements))))
                                    (t (setf elements (cons next-elt elements)))))))))
                       nil +bard-read-table+))

;;; end-of-sequence
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\]
                       (lambda (stream char)
                         (declare (ignore stream char))
                         (end-of-sequence))
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
                                    ((end-of-file? next-elt)(error "Unexpected end of input while reading a map"))
                                    ((end-of-map? next-elt) 
                                     (return-from reading  (cons :map (reverse elements))))
                                    (t (setf elements (cons next-elt elements)))))))))
                       nil +bard-read-table+))

;;; end-of-map
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\}
                       (lambda (stream char)
                         (declare (ignore stream char))
                         (end-of-map))
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
                          (if (and (not (eql *readtable* bard::+bard-read-table+)) ; special case for the Bard readtable 
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

(defmethod syntax-for (x)
  (error "invalid input to read: ~s" x))

(defmethod syntax-for ((x end-of-sequence))
  x)

(defmethod syntax-for ((x end-of-map))
  x)

(defmethod syntax-for ((x number))
  `(:number ,x))

(defmethod syntax-for ((x string))
  `(:string ,x))

(defmethod syntax-for ((x cons))
  (let ((stype (first x)))
    (cond
      ((eql stype :character) x)
      ((eql stype :sequence) x)
      ((eql stype :sequence-literal) x)
      ((eql stype :map) x)
      (t (error "invalid input to read: ~S" x)))))

(defmethod syntax-for ((x symbol))
  (let ((sname (symbol-name x)))
    (cond
      ((string= "nothing" sname)(list :nothing))
      ((string= "true" sname)(list :boolean :true))
      ((string= "false" sname)(list :boolean :false))
      (t (let ((colon-pos (position #\: sname)))
           (if colon-pos
               (let ((colon-pos2 (position #\: sname :from-end t)))
                 (if (equal colon-pos colon-pos2)
                     (let* ((mname (subseq sname 0 colon-pos))
                            (module-name (if (> (length mname) 0)
                                             mname
                                             "bard.keyword"))
                            (symbol-name (subseq sname (1+ colon-pos))))
                       (list :symbol module-name symbol-name))
                     (error "multiple colons in a symbol: ~S" x)))
               (list :symbol nil sname)))))))


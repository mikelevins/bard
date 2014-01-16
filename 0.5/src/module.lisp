;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          module.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard modules
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------

(defmethod bard-symbol ((name string))
  (cl:intern name :bard))

(defmethod bard-symbol ((name symbol))
  (cl:intern (symbol-name name) :bard))

(defmethod valid-module-name? (x) nil)

(defmethod valid-module-name? ((x symbol)) 
  (valid-module-name? (symbol-name x)))

(defmethod module-name-constituent? ((ch character))
  (find ch "1234567890-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ." :test 'char=))

(defmethod valid-module-name? ((x string)) 
  (and (not (zerop (length x)))
       (alpha-char-p (elt x 0))
       (every 'module-name-constituent? x)))

(defmethod module-name ((name string))
  (if (valid-module-name? name)
      (bard-symbol name)
      (error "Invalid module name: ~A" name)))

(defmethod module-name ((name symbol))
  (module-name (symbol-name name)))

;;; ---------------------------------------------------------------------
;;; modules 
;;; ---------------------------------------------------------------------

(defclass module ()
  ((symbols :accessor %symbols :initform {} :initarg :symbols)
   (exports :accessor %exports :initform {} :initarg :exports)))

(defmethod add-symbol! ((module module)(name symbol) &key (exported nil))
  (let ((sname (bard-symbol name)))
    (setf (%symbols module)
          (merge (%symbols module)
                 {sname {:name sname :exported exported}}))
    (when exported
      (setf (%exports module)
            (merge (%exports module)
                   {sname t})))))

(defmethod add-symbol! ((module module)(name string) &key (exported nil))
  (add-symbol! module (bard-symbol name) :exported exported))

(defmethod remove-symbol! ((module module)(name symbol))
  (let ((sname (bard-symbol name)))
    (setf (%symbols module)
          (remove-key (%symbols module) sname))
    (when exported
      (setf (%exports module)
            (remove-key (%exports module) sname)))))

(defmethod remove-symbol! ((module module)(name string))
  (remove-symbol! module (bard-symbol name)))

(defmethod find-symbol ((module module)(name symbol))
  (get-key (%symbols module) name))

(defmethod find-symbol ((module module)(name string))
  (find-symbol module (bard-symbol name)))

(defmethod parse-symbol-spec (s) (error "Invalid symbol spec: ~s" s))
(defmethod parse-symbol-spec ((s string)) {:name (bard-symbol s)})
(defmethod parse-symbol-spec ((s symbol)) {:name (bard-symbol s)})
(defmethod parse-symbol-spec ((s fset:map))
  (let ((name (get-key s :name)))
    (if name 
        (merge s {:name (bard-symbol name)})
        (error "Invalid symbol spec (no symbol name): ~s" s))))

(defun make-module (&key symbols exports)
  (let ((records (mapcar 'parse-symbol-spec symbols))
        (symbol-map {})
        (export-map {}))
    (loop for record in records 
       do (let* ((sname (get-key record :name))
                 (exported? (and (member sname exports :test 'string=) t))
                 (record* (merge record {:exported exported?})))
            (setf symbol-map (merge symbol-map {sname record*}))
            (when exported? (setf export-map (merge export-map {sname t})))))
    (make-instance 'module :symbols symbol-map :exports export-map)))

(defmethod defmodule ((name symbol) &key symbols exports)
  (setf *modules* (merge *modules*
                         {(module-name name)
                          (make-module :symbols symbols :exports exports)})))

(defmethod defmodule ((name string) &key symbols exports)
  (defmodule (bard-symbol name) :symbols symbols :exports exports))

(defmethod module-exports ((module module))
  (fset:convert 'cl:list (keys (%exports module))))

;;; ---------------------------------------------------------------------
;;; the global module registry
;;; ---------------------------------------------------------------------
;;; TODO: make these operations thread-safe

(defparameter *modules* {})
(defparameter *module* nil)

(defmethod assert-module! ((name string)(mod module))
  (setf *modules*
        (merge *modules* 
               {(module-name name) mod})))

(defmethod retract-module! ((name string)(mod module))
  (setf *modules* (remove-key *modules* (module-name name))))

(defmethod find-module ((name string))
  (get-key *modules* (module-name name)))

(defmethod in-module ((name string))
  (let ((mod (find-module name)))
    (if mod
        (setf *module* mod)
        (error "Module ~a not found" name))))

(defmethod import! ((to-module string)(from-module string)(name symbol) &key as export)
  (let* ((as-name (or as name))
         (tom (or (find-module to-module)
                  (error "No module named ~a is defined" to-module)))
         (fromm (or (find-module from-module)
                    (error "No module named ~a is defined" from-module)))
         (original-record (or (find-symbol fromm name)
                              (error "No symbol named ~a found in module ~a" name from-module))))
    (if (get-key original-record :exported)
        (let ((record {:name as-name :exported export :imported-from from-module :original-name name}))
          (setf (%symbols tom)
                (merge (%symbols tom) {as-name record}))
          (when export (setf (%exports tom)
                             (merge (%exports tom)
                                    {as-name t}))))
        (error "The symbol named ~a is not exported from module ~a" name from-module))
    as-name))

;;; ---------------------------------------------------------------------
;;; standard Bard modules
;;; ---------------------------------------------------------------------

(defmodule "bard.base"
    :symbols '("abort" "and" 
               "begin"
               "case" "catch" "class" "cond" "condition" "constant"
               "define" "do" "dolist" "dotimes"
               "ensure" "eval"
               "function"
               "generate"
               "handler-bind" "handler-case"
               "if" "invoke-restart"
               "let" "loop"
               "macro" "match" "method"
               "next-method"
               "or"
               "receive" "record" "restart-bind" "restart-case"
               "send" "set!" "setter" "signal"
               "the" "throw" "tuple"
               "union" "unless"
               "values" "variable"
               "when" "with-exit" "with-open")
    :exports '("abort" "and" 
               "begin"
               "case" "catch" "class" "cond" "condition" "constant"
               "define" "do" "dolist" "dotimes"
               "ensure" "eval"
               "function"
               "generate"
               "handler-bind" "handler-case"
               "if" "invoke-restart"
               "let" "loop"
               "macro" "match" "method"
               "next-method"
               "or"
               "receive" "record" "restart-bind" "restart-case"
               "send" "set!" "setter" "signal"
               "the" "throw" "tuple"
               "union" "unless"
               "values" "variable"
               "when" "with-exit" "with-open"))

(defmodule "bard.user")

(dolist (sym (module-exports (find-module "bard.base")))
  (import! "bard.user" "bard.base" sym :export t))

;;; ---------------------------------------------------------------------
;;; testing
;;; ---------------------------------------------------------------------
;;; (find-module "bard.base")
;;; (find-module "bard.user")


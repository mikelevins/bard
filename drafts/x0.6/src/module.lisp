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
;;; constructor helpers
;;; ---------------------------------------------------------------------

(defmethod valid-module-name-constituent? ((ch character))
  (find ch ".-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890" :test #'char=))

(defmethod valid-module-name? (x) nil)

(defmethod valid-module-name? ((name string))
  (and (not (zerop (length name)))
       (alpha-char-p (elt name 0))
       (every #'valid-module-name-constituent? name)))

(defmethod ensure-valid-module-name ((name string))
  (or (and (valid-module-name? name)
           name)
      (error "Invalid module name: ~S" name)))

;;; ---------------------------------------------------------------------
;;; module class
;;; ---------------------------------------------------------------------

(defclass module ()
  ((name :accessor module-name :initarg :name)
   ;; symbol->name
   (symbols :accessor %symbols :initform (make-hash-table) :initarg :symbols)
   ;; name->symbol
   (names :accessor %names :initform (make-hash-table :test #'equal) :initarg :names)
   (exports :accessor %exports :initform (make-hash-table :test #'equal) :initarg :exports)
   (imports :accessor %imports :initform (make-hash-table :test #'equal) :initarg :imports)
   (used-modules :accessor %used-modules :initform (make-hash-table :test #'equal) :initarg :used-modules)))

(defmethod print-object ((module module)(out stream))
  (princ "#<module " out)
  (princ (module-name module) out)
  (princ ">" out))

;;; ---------------------------------------------------------------------
;;; finding and fetching symbols
;;; ---------------------------------------------------------------------

(defmethod find-symbol ((name string)(module module))
  (gethash name (%names module)))

(defmethod symbol-names ((module module))
  (sort (loop for key being the hash-keys in (%names module) collect key)
        #'string<))

(defmethod exported-symbol-names ((module module))
  (sort (loop for key being the hash-keys in (%exports module) collect key)
        #'string<))

(defmethod used-modules ((module module))
  (sort (loop for key being the hash-keys in (%used-modules module) collect key)
        #'string<))

;;; ---------------------------------------------------------------------
;;; interning symbols
;;; ---------------------------------------------------------------------

(defmethod symbol-name ((symbol symbol) &optional (module nil))
  (let ((module (or module (symbol-module symbol))))
    (gethash symbol (%symbols module))))

(defmethod intern ((name string)(module module) &key (as nil)(export nil))
  (let ((local-name (or as name)))
    (if (equal name local-name)
        (let ((sym (make-instance 'symbol :module module)))
          (setf (gethash sym (%symbols module)) local-name)
          (setf (gethash local-name (%names module)) sym)
          (when export 
            (setf (gethash local-name (%exports module)) sym))
          sym)
        (error "Tried to intern a string (~S) as a different string (~S)"
               name as))))

(defmethod intern ((sym symbol)(module module) &key (as nil)(export nil))
  (let ((local-name (or as (symbol-name sym))))
    (setf (gethash sym (%symbols module)) local-name)
    (setf (gethash local-name (%names module)) sym)
    (when export 
      (setf (gethash local-name (%exports module)) sym))
    sym))

(defmethod in-module? ((symbol symbol)(module module))
  (multiple-value-bind (val found?)(gethash symbol (%symbols module))
    (declare (ignore val))
    found?))

;;; ---------------------------------------------------------------------
;;; exporting symbols
;;; ---------------------------------------------------------------------

(defmethod export ((sym symbol) &optional (module nil))
  (let ((module (or module (symbol-module sym))))
    (if (in-module? sym module)
        (let ((key (symbol-name sym module)))
          (setf (gethash (%exports module) key) sym)
          sym)
        (error "No such symbol ~S in module ~S" sym module))))

(defmethod export ((name string) &optional (module nil))
  (let* ((module (or module (current-module)))
         (sym (find-symbol name module)))
    (if sym
        (progn
          (setf (gethash (%exports module) name) sym)
          sym)
        (error "No such symbol ~S in module ~S" sym module))))

(defmethod exported? ((sym symbol) &optional (module nil))
  (let ((module (or module (symbol-module sym))))
    (if (in-module? sym module)
        (gethash (symbol-name sym module) (%exports module) nil)
        (error "No such symbol ~S in module ~S" sym module))))

(defmethod exported? ((name string) &optional (module nil))
  (let* ((module (or module (current-module)))
         (sym (gethash name (%names module))))
    (if sym
        (gethash name (%exports module) nil)
        (error "No such symbol ~S in module ~S" name module))))

;;; ---------------------------------------------------------------------
;;; importing symbols
;;; ---------------------------------------------------------------------

(defmethod import ((sym symbol) &key (to-module nil)(from-module nil)(as nil))
  (let* ((to-module (or to-module (current-module)))
         (from-module (or from-module (symbol-module sym)))
         (from-name (symbol-name sym from-module))
         (to-name (or as from-name)))
    (assert (exported? from-name from-module)()
            "Symbol ~S is not exported from module ~S"
            from-name (module-name from-module))
    (setf (gethash to-name (%imports to-module)) sym)
    (setf (gethash to-name (%names to-module)) sym)
    (setf (gethash sym (%symbols to-module)) to-name)
    sym))

(defmethod import ((name string) &key (to-module nil)(from-module nil)(as nil))
  (assert from-module ()
          "Tried to import a symbol by name (~s) without supplying a source module"
          name)
  (let* ((to-module (or to-module (current-module)))
         (from-name (symbol-name sym from-module))
         (to-name (or as from-name)))
    (assert (exported? from-name from-module)()
            "Symbol ~S is not exported from module ~S"
            from-name (module-name from-module))
    (setf (gethash to-name (%imports to-module)) sym)
    (setf (gethash to-name (%names to-module)) sym)
    (setf (gethash sym (%symbols to-module)) to-name)
    sym))

;;; ---------------------------------------------------------------------
;;; using modules
;;; ---------------------------------------------------------------------

(defmethod use-module ((module-to-use module) &optional (module nil))
  (let ((destination-module (or module (current-module)))
        (exported-names (exported-symbol-names module-to-use)))
    (dolist (name exported-names)
      (let ((sym (gethash name (%names module-to-use))))
        (import sym :to-module destination-module :from-module module-to-use)))
    (setf (gethash (module-name module-to-use)
                   (%used-modules destination-module))
          module-to-use)
    destination-module))

;;; ---------------------------------------------------------------------
;;; module registry
;;; ---------------------------------------------------------------------

(defparameter *modules* (fset:wb-map))
(defparameter *module* (undefined))

(defun current-module () *module*)

(defmethod register-module ((name string)(module module))
  (let ((mname (ensure-valid-module-name name)))
    (assert (equal mname (module-name module))() 
            "Cannot register module named ~S under a different name (~S)"
            (module-name module) mname)
    (setf *modules* (fset:with *modules* name module))
    mname))

(defmethod find-module ((name string))
  (fset:@ *modules* name))

(defparameter +bard-base-exports+
  '("abort" "and"
    "begin"
    "case" "catch" "class" "cond" "condition" "constant"
    "define"
    "do" "dolist" "dotimes" 
    "ensure" "eval" 
    "function"
    "generate"
    "handler-bind" "handler-case"
    "if" "invoke-restart"
    "let" "loop"
    "macro" "match" "method" 
    "next-method"
    "or"
    "receive" "record"
    "restart-bind" "restart-case"
    "send" "set!" "setter" "signal"
    "the" "throw" "tuple"
    "union" "unless"
    "values" "variable"
    "when" "with-exit" "with-open"))

(defun init-modules ()
  (register-module "bard.base" (make-instance 'module :name (ensure-valid-module-name "bard.base")))
  (mapcar (lambda (nm)(intern nm (find-module "bard.base") :export t))
          +bard-base-exports+)
  (register-module "bard.user" (make-instance 'module :name (ensure-valid-module-name "bard.user")))
  (use-module (find-module "bard.base")
              (find-module "bard.user"))
  (setf *module* (find-module "bard.user")))

;;; (init-modules)
;;; (intern "Foo" (current-module))
;;; (intern "Bar" (find-module "bard.base"))

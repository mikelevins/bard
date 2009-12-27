;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ast.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       Abstract Syntax Tree representation
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bint)

;;; ------------------------------------------------------------
;;; expressions
;;; ------------------------------------------------------------

(defclass expression ()
  ((annotations :reader annotations :initarg :annotations)))

(defmethod expression? (x) 
  (declare (ignore x))
  nil)
(defmethod expression? ((x expression)) t)

(defclass value-expression (expression)
  ((value :reader value :initarg :value)))

(defmethod value-expression? (x) 
  (declare (ignore x))
  nil)
(defmethod value-expression? ((x value-expression)) t)

;;; Void

(defclass void-expression (value-expression)())

(defun void ()(make-instance 'void-expression))

(defmethod void? (x) 
  (declare (ignore x))
  nil)
(defmethod void? ((x void-expression)) t)

(defmethod print-object ((obj void-expression) str)
  (format str "<void>"))

;;; Number

(defclass number-expression (value-expression)())

(defmethod number ((n cl:number))
  (make-instance 'number-expression :value n))

(defmethod number? (x)
  (declare (ignore x))
  nil)
(defmethod number? ((x number-expression)) t)

(defmethod print-object ((obj number-expression) str)
  (format str "<~d>" (value obj)))

;;; Character

(defclass character-expression (value-expression)())

(defmethod character ((n cl:character))
  (make-instance 'character-expression :value n))

(defmethod character? (x)
  (declare (ignore x))
  nil)
(defmethod character? ((x character-expression)) t)

(defmethod print-object ((obj character-expression) str)
  (format str "<~A>" (value obj)))

;;; Symbol

(defclass symbol-expression (value-expression)())

(defmethod symbol ((s string))
  (make-instance 'symbol-expression :value s))

(defmethod symbol? (x) 
  (declare (ignore x))
  nil)
(defmethod symbol? ((x symbol-expression)) t)

(defmethod print-object ((obj symbol-expression) str)
  (format str "<~a>" (value obj)))

;;; Keyword

(defclass keyword-expression (value-expression)())

(defmethod keyword ((s string))
  (make-instance 'keyword-expression :value s))

(defmethod keyword? (x)
  (declare (ignore x))
  nil)
(defmethod keyword? ((x keyword-expression)) t)

(defmethod print-object ((obj keyword-expression) str)
  (format str "<~a:>" (value obj)))

;;; Boolean

(defclass boolean-expression (value-expression)())
(defmethod boolean? (x)
  (declare (ignore x))
  nil)
(defmethod boolean? ((x boolean-expression)) t)

;;; True

(defclass true-expression (boolean-expression)())

(defun true ()(make-instance 'true-expression))

(defmethod true? (x)
  (declare (ignore x))
  nil)
(defmethod true? ((x true-expression)) t)

(defmethod print-object ((obj true-expression) str)
  (format str "<true>"))

;;; False

(defclass false-expression (boolean-expression)())

(defun false ()(make-instance 'false-expression))

(defmethod false? (x)
  (declare (ignore x))
  nil)
(defmethod false? ((x false-expression)) t)

(defmethod print-object ((obj false-expression) str)
  (format str "<false>"))

;;; Text

(defclass text-expression (value-expression)())

(defmethod text ((s string))
  (make-instance 'text-expression :value s))

(defmethod text? (x) 
  (declare (ignore x))
  nil)
(defmethod text? ((x text-expression)) t)

(defmethod print-object ((obj text-expression) str)
  (format str "<\"~a\">" (value obj)))

;;; Sequence

(defclass abstract-sequence-expression (expression)())

(defclass empty-sequence-expression (abstract-sequence-expression)())

(defmethod first ((x empty-sequence-expression))(void))
(defmethod rest ((x empty-sequence-expression))(void))

(defclass sequence-expression (abstract-sequence-expression)
  ((first :reader first :initarg :first)
   (rest :reader rest :initarg :rest)))

(defun empty-sequence ()
  (make-instance 'empty-sequence-expression))

(defun sequence (&rest vals)
  (if (null vals)
      (empty-sequence)
      (if (expression? (cl:first vals))
          (make-instance 'sequence-expression
                         :first (cl:first vals)
                         :rest (apply 'sequence (cl:rest vals)))
          (error "Can't create a sequence; value is not an expression: ~S" (cl:first vals)))))

(defmethod empty-sequence? (x) 
  (declare (ignore x))
  nil)
(defmethod empty-sequence? ((x empty-sequence-expression)) t)

(defmethod sequence? (x) 
  (declare (ignore x))
  nil)
(defmethod sequence? ((x abstract-sequence-expression)) t)

(defmethod print-object ((obj empty-sequence-expression) str)
  (format str "<()>"))

(defmethod %print-sequence-elements ((s empty-sequence-expression) str)
  (declare (ignore s str))
  nil)

(defmethod %print-sequence-elements ((s sequence-expression) str)
  (let ((hd (first s))
        (tl (rest s)))
    (print-object hd str)
    (unless (empty-sequence? tl)
        (format str " "))
    (%print-sequence-elements tl str)))

(defmethod print-object ((obj sequence-expression) str)
  (format str "<(")
  (%print-sequence-elements obj str)
  (format str ")>"))

;;; Map

(defclass abstract-map-expression (expression)())

(defclass empty-map-expression (abstract-map-expression)())

(defmethod first ((x empty-map-expression))(void))
(defmethod rest ((x empty-map-expression))(void))

(defclass map-expression (abstract-map-expression)
  ((entries :reader entries :initarg :entries)))

(defun empty-map ()
  (make-instance 'empty-map-expression))

(defun %plist->alist (plist)
  (if (null plist)
      plist
      (let ((k (car plist))
            (tl (cdr plist)))
        (if (expression? k)
            (if (null tl)
                (error "Odd number of arguments to map constructor")
                (let ((v (car tl))
                      (tl (cdr tl)))
                  (if (expression? v)
                      (cons (cons k v)
                            (%plist->alist tl))
                      (error "Not an expression: ~S" v))))
            (error "Not an expression: ~S" k)))))

(defun map (&rest keys-and-vals)
  (if (null keys-and-vals)
      (empty-map)
      (make-instance 'map-expression :entries (%plist->alist keys-and-vals))))

(defmethod empty-map? (x) 
  (declare (ignore x))
  nil)
(defmethod empty-map? ((x empty-map-expression)) t)

(defmethod map? (x) 
  (declare (ignore x))
  nil)
(defmethod map? ((x abstract-map-expression)) t)

(defmethod print-object ((obj empty-map-expression) str)
  (format str "<{}>"))

(defmethod %print-map-elements ((s empty-map-expression) str)
  (declare (ignore s str))
  nil)

(defun %print-map-entries (es str)
  (if (null es)
      nil
      (let* ((e (car es))
             (k (car e))
             (v (cdr e))
             (tl (cdr es)))
        (print-object k str)
        (format str " ")
        (print-object v str)
        (when (not (null tl))
          (format str " "))
        (%print-map-entries tl str))))

(defmethod %print-map-elements ((s map-expression) str)
  (%print-map-entries (entries s) str))

(defmethod print-object ((obj map-expression) str)
  (format str "<{")
  (%print-map-elements obj str)
  (format str "}>"))

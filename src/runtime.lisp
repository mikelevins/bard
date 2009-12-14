;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtime.lisp
;;;; Project:       bard
;;;; Purpose:       the Bard runtime library
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ----------------------------------------------------------------------
;;; Representation of Bard types
;;; ----------------------------------------------------------------------

(defclass bard-type ()
  ((type-name :reader type-name :initarg :type-name)))

(defclass primitive-type (bard-type)
  ((representation :accessor representation :initarg :representation)))

(defclass type-synonym (bard-type)
  ((base-type :accessor base-type :initarg :base-type)))

(defclass category (bard-type)
  ((members :accessor members :initarg :members)))

(defclass sequence-type (bard-type)())

(defclass structure-type (bard-type)())

;;; ----------------------------------------------------------------------
;;; representing values
;;; ----------------------------------------------------------------------

(defclass bard-value ()
  ((type :reader type-of :initarg :type)))

(defclass simple-value (bard-value)
  ((value :reader value :initarg :value)))

(defclass sequence-value (bard-value)())
(defclass vector-value (sequence-value)
  ((elements :reader elements :initarg :elements)))

;;; ----------------------------------------------------------------------
;;; Primitive types
;;; ----------------------------------------------------------------------

(defparameter <void>
  (make-instance 'primitive-type
                 :representation (class-of nil)
                 :type-name "<void>"))

(defparameter <true>
  (make-instance 'primitive-type
                 :representation (class-of t)
                 :type-name "<true>"))

(defparameter true (make-instance 'simple-value :type <true> :value t))

(defparameter <false>
  (make-instance 'primitive-type
                 :representation (class-of nil)
                 :type-name "<false>"))

(defparameter false (make-instance 'simple-value :type <false> :value nil))

(defparameter <keyword>
  (make-instance 'primitive-type
                 :representation (class-of :keyword)
                 :type-name "<keyword>"))

(defparameter <symbol>
  (make-instance 'primitive-type
                 :representation (find-class 'symbol)
                 :type-name "<symbol>"))

(defparameter <unicode-text>
  (make-instance 'primitive-type 
                 :representation (find-class 'simple-base-string)
                 :type-name "<unicode-text>"))

(defparameter <integer>
  (make-instance 'primitive-type 
                 :representation (find-class 'integer)
                 :type-name "<integer>"))

(defparameter <unicode-character>
  (make-instance 'primitive-type 
                 :representation (find-class 'character)
                 :type-name "<unicode-character>"))

(defparameter <empty-sequence>
  (make-instance 'sequence-type
                 :type-name "<empty-sequence>"))

(defparameter <vector>
  (make-instance 'sequence-type
                 :type-name "<vector>"))

;;; ----------------------------------------------------------------------
;;; Categories
;;; ----------------------------------------------------------------------

(defparameter <Boolean>
  (make-instance 'category
                 :members (list <true> <false>)
                 :type-name "<Boolean>"))

(defparameter <Character>
  (make-instance 'category
                 :members (list <unicode-character>)
                 :type-name "<Character>"))

(defparameter <Name>
  (make-instance 'category
                 :members (list <keyword> <symbol>)
                 :type-name "<Name>"))

(defparameter <Number>
  (make-instance 'category
                 :members (list <integer>)
                 :type-name "<Number>"))

(defparameter <Text>
  (make-instance 'category
                 :members (list <unicode-text>)
                 :type-name "<Text>"))

(defparameter <Sequence>
  (make-instance 'category
                 :members (list <vector> <Text>)
                 :type-name "<Sequence>"))

;;; ----------------------------------------------------------------------
;;; value constructors for primitive types
;;; ----------------------------------------------------------------------

(defmethod make ((btype bard-type) &rest inits)
  (error "No constructor defined for type ~S" btype))

(defparameter <unrecognized-type> 
  (make-instance 'primitive-type 
                 :representation nil
                 :type-name "<unrecognized-type>"))

(defmethod make ((a-type (eql <unicode-text>)) &rest inits)
  (let ((val (getf inits :value)))
    (make-instance 'simple-value :type <unicode-text> :value val)))

(defmethod make ((a-type (eql <Text>)) &rest inits)
  (apply 'make `(,<unicode-text> ,@inits)))

(defmethod make ((a-type (eql <integer>)) &rest inits)
  (let ((val (getf inits :value)))
    (make-instance 'simple-value :type <integer> :value val)))

(defmethod make ((a-type (eql <Number>)) &rest inits)
  (let ((val (getf inits :value)))
    (cond
      ((integerp val) (apply 'make `(,<integer> ,@inits)))
      (t (make <unrecognized-type>)))))

(defmethod make ((a-type (eql <unicode-character>)) &rest inits)
  (let ((val (getf inits :value)))
    (make-instance 'simple-value :type <unicode-character> :value val)))

(defmethod make ((a-type (eql <Character>)) &rest inits)
  (let ((val (getf inits :value)))
    (apply 'make `(,<unicode-character> ,@inits))))

(defmethod make ((a-type (eql <true>)) &rest inits) true)

(defmethod make ((a-type (eql <false>)) &rest inits) false)

(defmethod make ((a-type (eql <keyword>)) &rest inits)
  (let ((val (getf inits :value)))
    (make-instance 'simple-value :type <keyword> :value val)))

(defmethod make ((a-type (eql <symbol>)) &rest inits)
  (let ((val (getf inits :value)))
    (make-instance 'simple-value :type <symbol> :value val)))

;;; ----------------------------------------------------------------------
;;; sequences
;;; ----------------------------------------------------------------------
;;; NOTE: I'll start with simple, naive sequence types based on ordinary
;;; analogous Lisp data structures. Without measuring, I assume these will
;;; inefficient in the extreme, since the vast majority of operations
;;; will treat them as immutable, and therefore will cons like crazy.
;;; But it's must easier to build this subsystem using built-in data 
;;; structures, and it also provides the opportunity to measure just how
;;; much is to be gained by replacing them with more sophisticated 
;;; data structures such os Okasaki's, or those used in Clojure.


;;; <vector>

(defmethod make ((a-type (eql <vector>)) &rest inits)
  (let ((elts (getf inits :elements)))
    (make-instance 'vector-value
                   :type <vector>
                   :elements (apply 'vector elts))))

;;; ----------------------------------------------------------------------
;;; sequence library
;;; ----------------------------------------------------------------------

;;; <Sequence>

(defmethod count ((val vector-value))
  (length (elements val)))

(defmethod element ((v vector-value)(index integer))
  (elt (elements v) index))

(defmethod empty? ((val sequence-value))
  (and (zerop (count val)) true))

;;; ----------------------------------------------------------------------
;;; text utils
;;; ----------------------------------------------------------------------

(defmethod to-bard-text ((val bard-value))
  "#<a bard value of type ~A>" (type-of val))

(defmethod %simple-value->text (a-type (val simple-value))
  (format nil "~S" (value val)))

(defmethod %simple-value->text ((tp (eql <symbol>))(val simple-value))
  (format nil "~A" (value val)))

(defmethod to-bard-text ((val simple-value))
  (%simple-value->text (type-of val) val))

(defmethod to-bard-text ((obj vector-value))
  (with-output-to-string (stream)
    (format stream "(")
    (let ((item-count (count obj)))
      (when (> item-count 0)
        (dotimes (i (- item-count 1))
          (let ((it (element obj i)))
            (print-object it stream)
            (format stream " ")))
        (print-object (element obj (- item-count 1)) stream)))
    (format stream ")")))
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structures.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard structures
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; structure representation
;;; ---------------------------------------------------------------------

(defclass bard-structure ()
  ((name :accessor %name :initarg :name)))

(defclass bard-primitive-structure (bard-structure)
  ((lisp-type :accessor %lisp-type :initarg :lisp-type)))

(defclass bard-record (bard-structure)
  ((slots :accessor %slots :initarg :slots)))

(defclass bard-tuple (bard-structure)
  ((min-count :accessor %min-count :initarg :min-count)
   (max-count :accessor %max-count :initarg :max-count)
   (element-type :accessor %element-type :initarg :element-type)))

(defclass bard-union (bard-structure)
  ((member-types :accessor %member-types :initarg :member-types)))

(defclass bard-enumeration (bard-structure)
  ((member-values :accessor %member-values :initarg :member-values)))

(defmethod print-object ((rec bard-primitive-structure)(s stream))
  (princ "#[<primitive-structure>] " s)
  (princ (%name rec) s))

(defmethod print-object ((rec bard-record)(s stream))
  (princ "#[<record>] " s)
  (princ (%name rec) s))

(defmethod print-object ((tup bard-tuple)(s stream))
  (princ "#[<tuple>] " s)
  (princ (%name tup) s))

(defmethod print-object ((un bard-union)(s stream))
  (princ "#[<union>] " s)
  (princ (%name un) s))

(defmethod print-object ((enum bard-enumeration)(s stream))
  (princ "#[<enumeration>] " s)
  (princ (%name enum) s))

(defun record (name slots)
  (make-instance 'bard-record :name name :slots slots))

(defun tuple (name min max type)
  (make-instance 'bard-tuple :min-count min :max-count max :element-type type))

(defun union (name members)
  (make-instance 'bard-union :name name :member-types members))

(defun enumeration (name members)
  (make-instance 'bard-enumneration :name name :member-values members))

(defmacro define-primitive-structure (name lisp-type)
  `(defparameter ,name (make-instance 'bard-primitive-structure :name ',name :lisp-type ',lisp-type)))

(defmacro define-record (name &rest slots)
  (let ((slots (loop for plist on slots by 'cddr collect (list 'cl:cons (cons 'cl:quote (car plist))(cadr plist)))))
    `(defparameter ,name (make-instance 'bard-record :name ',name :slots (cl:list ,@slots))a)))

(defmacro define-tuple (name &key (minimum-count 0)(maximum-count nil)(element-type '|Anything|))
  `(defparameter ,name (make-instance 'bard-tuple :name ',name :minimum-count ,minimum-count 
                                      :maximum-count ,maximum-count :element-type ',element-type)))

(defmacro define-union (name &key (member-types nil))
  `(defparameter ,name (make-instance 'bard-union :name ',name :member-types ',member-types)))

(defmacro define-enumeration (name &key (member-values nil))
  `(defparameter ,name (make-instance 'bard-enumeration :name ',name :member-values (list ,@member-values))))

;;; ---------------------------------------------------------------------
;;; class definitions
;;; ---------------------------------------------------------------------

;;; <adjustable-object-vector>
(define-primitive-structure |<adjustable-object-vector>| '(array t *))

;;; <agent>

(defclass |<agent>| ()())
(define-primitive-structure |<agent>| '|<agent>|)

;;; <abort>
;;; <bard>
;;; <bignum>
(define-primitive-structure |<bignum>| 'bignum)

;;; <bit-vector>
(define-primitive-structure |<bit-vector>| '(simple-array (unsigned-byte 1) *))

;;; <buffer-input>
;;; <buffer-io>
;;; <buffer-output>
;;; <cons>
(define-primitive-structure |<cons>| 'cons)

;;; <complexnum>
(define-primitive-structure |<complexnum>| 'complex)

;;; <character>
(define-primitive-structure |<character>| 'character)

;;; <class>
(define-primitive-structure |<class>| 'class)

;;; <double-float>
(define-primitive-structure |<double-float>| 'double-float)

;;; <enumeration>
;;; <error>
;;; <false>
;;; <file-input>
;;; <file-io>
;;; <file-output>
;;; <fixnum>
(define-primitive-structure |<fixnum>| 'fixnum)

;;; <function>
;;; <gatherer>
;;; <generator>
;;; <getter>
;;; <hash-table>
(define-primitive-structure |<fixnum>| 'hash-table)

;;; <keyword>
;;; <message-input>
;;; <message-io>
;;; <message-output>
;;; <method>
;;; <module>
;;; <number-array>
;;; <number-vector>
;;; <network-output>
;;; <network-input>
;;; <network-io>
;;; <object-vector>
;;; <object-array>
;;; <posix-pathname>
;;; <ratio>
(define-primitive-structure |<ratio>| 'ratio)

;;; <read-table>
;;; <record>
;;; <restart>
;;; <setter>
;;; <single-float>
(define-primitive-structure |<single-float>| 'single-float)

;;; <singleton>
;;; <standard-error>
;;; <standard-input>
;;; <standard-output>
;;; <string>
(define-primitive-structure |<string>| 'string)

;;; <symbol>
(define-primitive-structure |<symbol>| 'symbol)

;;; <symbolic-pathname>
;;; <time>
;;; <true>
;;; <tuple>
;;; <url>
;;; <union>
;;; <warning>
;;; <weight-balanced-map>
;;; <weight-balanced-sequence>
;;; <weight-balanced-string>


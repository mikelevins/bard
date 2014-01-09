;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classes.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard classes
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; class representation
;;; ---------------------------------------------------------------------

(defclass bard-class ()
  ((name :accessor %name :initarg :name)
   (protocol :accessor %protocol :initarg :protocol)
   (superclasses :accessor %superclasses :initarg :superclasses)))

(defmethod print-object ((class bard-class)(s stream))
  (princ (%name class) s))

(defun protocol (&rest plist) plist)

(defun class (name superclasses protocol)
  (make-instance 'bard-class :name name :superclasses superclasses :protocol protocol))

(defmacro define-class (name superclasses &rest protocol)
  `(defparameter ,name (class ',name ',superclasses (protocol ,@protocol))))

;;; ---------------------------------------------------------------------
;;; class definitions
;;; ---------------------------------------------------------------------

;;; Abort

(define-class |Abort| (|Condition|))

;;; Accessor

(define-class |Accessor| (|Procedure|))

;;; Adjustable

(define-class |Adjustable| (|Anything|))

;;; Agent

(define-class |Agent| (|MessageStream|))

;;; Anything

(define-class |Anything| ())

;;; Array

(define-class |Array| (|Collection|))

;;; Bard

(define-class |Bard| (|Agent|))

;;; Boolean

(define-class |Boolean| (|Anything|))

;;; BufferStream

(define-class |BufferStream| (|Stream|))

;;; Character

(define-class |Character| (|Ordered|))

;;; Class

(define-class |Class| (|Type|))

;;; Collection

(define-class |Collection| (|Anything|))

;;; ComputeStream

(define-class |ComputeStream| (|Stream|))

;;; Complex

(define-class |Complex| (|Number|))

;;; Condition

(define-class |Condition| (|Anything|))

;;; Consumer

(define-class |Consumer| (|Stream|))

;;; Duration

(define-class |Duration| (|Ordered|))

;;; Error

(define-class |Error| (|Condition|))

;;; False

(define-class |False| (|Boolean|))

;;; FileStream

(define-class |FileStream| (|Stream|))

;;; Float

(define-class |Float| (|Real|))

;;; Function

(define-class |Function| (|Procedure|))

;;; Getter

(define-class |Getter| (|Accessor|))

;;; Integer

(define-class |Integer| (|Rational|))

;;; List

(define-class |List| (|Collection|))

;;; Map

(define-class |Map| (|Collection|))

;;; MessageStream

(define-class |MessageStream| (|Stream|))

;;; Method

(define-class |Method| (|Procedure|))

;;; Mutable

(define-class |Mutable| (|Anything|))

;;; Name

(define-class |Name| (|Anything|))

;;; NetworkStream

(define-class |NetworkStream| (|Stream|))

;;; Number

(define-class |Number| (|Ordered|))

;;; Ordered

(define-class |Ordered| (|Anything|))

;;; Pair

(define-class |Pair| (|Collection|))

;;; Pathname

(define-class |Pathname| (|ResourceName|))

;;; PosixPathname

(define-class |PosixPathname| (|Pathname|)
  '|path| (-> |PosixPathname| -> |Text|))

;;; Procedure

(define-class |Procedure| (|Anything|)
  '|apply| (-> |Procedure| |List| -> |Anything|))

;;; Producer

(define-class |Producer| (|Stream|))

;;; Ratio

(define-class |Ratio| (|Rational|))

;;; Rational

(define-class |Rational| (|Real|))

;;; Real

(define-class |Real| (|Number|))

;;; ResourceName

(define-class |ResourceName| (|Name|))

;;; Restart

(define-class |Restart| (|Condition|))

;;; Setter

(define-class |Setter| (|Accessor|))

;;; Singleton

(define-class |Singleton| (|Type|))

;;; StandardIOStream

(define-class |StandardIOStream| (|Stream|))

;;; Stream

(define-class |Stream| (|Collection|))

;;; Structure

(define-class |Structure| (|Type|))

;;; Symbol

(define-class |Symbol| (|Name|))

;;; SymbolicPathname

(define-class |SymbolicPathname| (|Pathname|))

;;; Text

(define-class |Text| (|Vector| |Ordered|))

;;; Time

(define-class |Time| (|Ordered|))

;;; True

(define-class |True| (|Boolean|))

;;; Type

(define-class |Type| (|Anything|))

;;; TypeRestricted

(define-class |TypeRestricted| (|Anything|))

;;; URL

(define-class |URL| (|ResourceName|)
  '|path| (-> |PosixPathname| -> |Text|)
  '|scheme| (-> |PosixPathname| -> |Text|)
  '|host| (-> |PosixPathname| -> |Text|)
  '|port| (-> |PosixPathname| -> |Integer|)
  '|query| (-> |PosixPathname| -> |Text|))

;;; Vector

(define-class |Vector| (|Array| |List|))

;;; Warning

(define-class |Warning| (|Condition|))



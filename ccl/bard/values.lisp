;;;; bard.lisp

(in-package :bard)

;;; =====================================================================
;;; singletons
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; <eof>->Eof
;;; ---------------------------------------------------------------------

(defclass <eof> ()()(:metaclass singleton-class))

(defmethod print-object ((eof <eof>)(str stream))
  (format str "#<eof>"))

(defun eof ()(make-instance '<eof>))
(defmethod eof? (thing)(declare (ignore thing)) nil)
(defmethod eof? ((thing <eof>))(declare (ignore thing)) t)

;;; ---------------------------------------------------------------------
;;; <undefined>->Undefined
;;; ---------------------------------------------------------------------

(defclass <undefined> ()()(:metaclass singleton-class))

(defmethod print-object ((un <undefined>)(str stream))
  (format str "undefined"))

(defun undefined ()(make-instance '<undefined>))
(defmethod undefined? (thing)(declare (ignore thing)) nil)
(defmethod undefined? ((thing <undefined>))(declare (ignore thing)) t)
(defmethod defined? (thing)(not (undefined? thing)))

;;; ---------------------------------------------------------------------
;;; <anything>->Anything
;;; ---------------------------------------------------------------------

(defclass <anything> ()()(:metaclass singleton-class))

(defmethod print-object ((any <anything>)(str stream))
  (format str "anything"))

(defun anything ()(make-instance '<anything>))
(defmethod nothing? ((thing <anything>))(declare (ignore thing)) nil)
(defmethod something? ((thing <anything>))(declare (ignore thing)) t)

;;; ---------------------------------------------------------------------
;;; <nothing>->Nothing
;;; ---------------------------------------------------------------------

(defclass <nothing> ()()(:metaclass singleton-class))

(defmethod print-object ((no <nothing>)(str stream))
  (format str "nothing"))

(defun nothing ()(make-instance '<nothing>))
(defmethod nothing? (thing)(declare (ignore thing)) nil)
(defmethod nothing? ((thing <nothing>))(declare (ignore thing)) t)
(defmethod something? (thing)(not (nothing? thing)))

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(defclass <true> ()()(:metaclass singleton-class))

(defmethod print-object ((true <true>)(str stream))
  (format str "true"))

(defun true ()(make-instance '<true>))

(defclass <false> ()()(:metaclass singleton-class))

(defmethod print-object ((false <false>)(str stream))
  (format str "false"))

(defun false ()(make-instance '<false>))

(defmethod false? (thing)(declare (ignore thing)) nil)
(defmethod false? ((thing <false>))(declare (ignore thing)) t)
(defmethod false? ((thing <nothing>))(declare (ignore thing)) t)
(defmethod false? ((thing <undefined>))(declare (ignore thing)) t)
(defmethod false? ((thing null))(declare (ignore thing)) t)

(defmethod true? (thing)(not (false? thing)))

;;; =====================================================================
;;; Atom
;;; =====================================================================

;;; Type
;;; Representation->Type
(defclass <vector-representation> ()())
(defclass <record-representation> ()())
;;; Category->Type
(defclass <category> ()())

(deftype <name> () 'symbol)
(deftype <character> () 'standard-char)
(deftype <small-integer> () 'fixnum)
(deftype <big-integer> () 'bignum)
(deftype <single-float> () 'single-float)
(deftype <double-float> () 'double-float)

;;; =====================================================================
;;; Pair
;;; =====================================================================

(deftype <ratio> () 'ratio)

(defclass <slot> ()())

;;; =====================================================================
;;; Mapping
;;; =====================================================================

(deftype <module> () 'package)

(deftype <simple-mapping> () 'fset:wb-map)

;;; =====================================================================
;;; Sequence
;;; =====================================================================

(deftype <simple-sequence> () 'fset:wb-seq)

(deftype <bitvector> () 'simple-bit-vector)
(deftype <bytevector> () '(simple-array (unsigned-byte 8)))
(deftype <simple-string> () 'simple-base-string)

;;; =====================================================================
;;; Function
;;; =====================================================================

(defclass <primitive> ()())
(defclass <macroexpander> ()())
(defclass <generic-function> ()())
(defclass <method-function> ()())

;;; =====================================================================
;;; Process
;;; =====================================================================

(defclass <process> ()())
(defclass <cell> ()())

;;; Ports
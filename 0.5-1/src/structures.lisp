;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structures.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard structures
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; first-class structure objects
;;; ---------------------------------------------------------------------
;;;  (Bard values that represent structure types)

(defclass %bard-structure% ()
  ((name :accessor %structure-name :initarg :structure-name)))

(defmethod print-object ((struct %bard-structure%)(s stream))
  (princ (%structure-name struct) s))

(defclass %bard-structure-instance% ()())

;;; ---------------------------------------------------------------------
;;; structures that are aliases for standard Common Lisp types
;;; ---------------------------------------------------------------------

(defclass %lisp-type-structure% (%bard-structure%)
  ((lisp-type-specifier :accessor %lisp-type-specifier :initarg :type-specifier)))

(defmethod print-object ((obj %lisp-type-structure%)(s stream))
  (princ (symbol-name (%structure-name obj)) s))

;;; <bignum>
;;; ---------------------------------------------------------------------

(defparameter <bignum> (make-instance '%lisp-type-structure%
                                      :structure-name 'bard::|<bignum>|
                                      :type-specifier 'cl:bignum))

(defmethod %<bignum>? (x)(declare (ignore x)) nil)
(defmethod %<bignum>? ((x cl:bignum))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:bignum))(declare (ignore x)) <bignum>)

;;; <bitvector>
;;; ---------------------------------------------------------------------

(defparameter <bitvector> (make-instance '%lisp-type-structure%
                                         :structure-name 'bard::|<bitvector>|
                                         :type-specifier 'cl:bit-vector))

(defmethod %<bitvector>? (x) (typep x 'bit-vector))

(defmethod %bard-type-of ((x cl:bit-vector))(declare (ignore x)) <bitvector>)

;;; <cons>
;;; ---------------------------------------------------------------------

(defparameter <cons> (make-instance '%lisp-type-structure%
                                    :structure-name 'bard::|<cons>|
                                    :type-specifier 'cl:cons))

(defmethod %<cons>? (x)(declare (ignore x)) nil)
(defmethod %<cons>? ((x cl:cons))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:cons))(declare (ignore x)) <cons>)

;;; <double-float>
;;; ---------------------------------------------------------------------

(defparameter <double-float> (make-instance '%lisp-type-structure%
                                            :structure-name 'bard::|<double-float>|
                                            :type-specifier 'cl:double-float))

(defmethod %<double-float>? (x) (typep x 'cl:double-float))

(defmethod %bard-type-of ((x cl:double-float))(declare (ignore x)) <double-float>)

;;; <fixnum>
;;; ---------------------------------------------------------------------

(defparameter <fixnum> (make-instance '%lisp-type-structure%
                                      :structure-name 'bard::|<fixnum>|
                                      :type-specifier 'cl:fixnum))

(defmethod %<fixnum>? (x)(declare (ignore x)) nil)
(defmethod %<fixnum>? ((x cl:fixnum))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:fixnum))(declare (ignore x)) <fixnum>)

;;; <hash-table>
;;; ---------------------------------------------------------------------

(defparameter <hash-table> (make-instance '%lisp-type-structure%
                                          :structure-name 'bard::|<hash-table>|
                                          :type-specifier 'cl:hash-table))

(defmethod %<hash-table>? (x)(declare (ignore x)) nil)
(defmethod %<hash-table>? ((x cl:hash-table))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:hash-table))(declare (ignore x)) <hash-table>)


;;; <nothing>
;;; ---------------------------------------------------------------------

(defparameter <nothing> (make-instance '%lisp-type-structure%
                                       :structure-name 'bard::|<nothing>|
                                       :type-specifier 'cl:null))

(defmethod %<nothing>? (x)(declare (ignore x)) nil)
(defmethod %<nothing>? ((x cl:null))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:null))(declare (ignore x)) <nothing>)


;;; <package>
;;; ---------------------------------------------------------------------

(defparameter <package> (make-instance '%lisp-type-structure%
                                       :structure-name 'bard::|<package>|
                                       :type-specifier 'cl:package))

(defmethod %<package>? (x)(declare (ignore x)) nil)
(defmethod %<package>? ((x cl:package))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:package))(declare (ignore x)) <package>)

;;; <random-state>
;;; ---------------------------------------------------------------------

(defparameter <random-state> (make-instance '%lisp-type-structure%
                                            :structure-name 'bard::|<random-state>|
                                            :type-specifier 'cl:random-state))

(defmethod %<random-state>? (x)(declare (ignore x)) nil)
(defmethod %<random-state>? ((x cl:random-state))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:random-state))(declare (ignore x)) <random-state>)

;;; <readtable>
;;; ---------------------------------------------------------------------

(defparameter <readtable> (make-instance '%lisp-type-structure%
                                         :structure-name 'bard::|<readtable>|
                                         :type-specifier 'cl:readtable))

(defmethod %<readtable>? (x)(declare (ignore x)) nil)
(defmethod %<readtable>? ((x cl:readtable))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:readtable))(declare (ignore x)) <readtable>)

;;; <restart>
;;; ---------------------------------------------------------------------

(defparameter <restart> (make-instance '%lisp-type-structure%
                                       :structure-name 'bard::|<restart>|
                                       :type-specifier 'cl:restart))

(defmethod %<restart>? (x)(declare (ignore x)) nil)

(defmethod %bard-type-of ((x cl:restart))(declare (ignore x)) <restart>)

;;; <single-float>
;;; ---------------------------------------------------------------------

(defparameter <single-float> (make-instance '%lisp-type-structure%
                                            :structure-name 'bard::|<single-float>|
                                            :type-specifier 'cl:single-float))

(defmethod %<single-float>? (x) (typep x 'cl:single-float))

(defmethod %bard-type-of ((x cl:single-float))(declare (ignore x)) <single-float>)

;;; <string>
;;; ---------------------------------------------------------------------

(defparameter <string> (make-instance '%lisp-type-structure%
                                      :structure-name 'bard::|<string>|
                                      :type-specifier 'cl:string))

(defmethod %<string>? (x)(declare (ignore x)) nil)
(defmethod %<string>? ((x cl:string))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:string))(declare (ignore x)) <string>)

;;; <vector>
;;; ---------------------------------------------------------------------

(defparameter <vector> (make-instance '%lisp-type-structure%
                                      :structure-name 'bard::|<vector>|
                                      :type-specifier 'cl:simple-vector))

(defmethod %<vector>? (x)(declare (ignore x)) nil)
(defmethod %<vector>? ((x cl:simple-vector))(declare (ignore x)) t)

(defmethod %bard-type-of ((x cl:simple-vector))(declare (ignore x)) <vector>)

;;; <wordvector>
;;; ---------------------------------------------------------------------

(defparameter <wordvector> (make-instance '%lisp-type-structure%
                                          :structure-name 'bard::|<wordvector>|
                                          :type-specifier 'cl:simple-array))

(defmethod %<wordvector>? (x)(declare (ignore x)) nil)
(defmethod %<wordvector>? ((x ccl::simple-byte-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-unsigned-byte-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-word-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-unsigned-word-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-long-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-unsigned-long-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-doubleword-vector))(declare (ignore x)) t)
(defmethod %<wordvector>? ((x ccl::simple-unsigned-doubleword-vector))(declare (ignore x)) t)

(defmethod %bard-type-of ((x ccl::simple-byte-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-unsigned-byte-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-word-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-unsigned-word-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-long-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-unsigned-long-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-doubleword-vector))(declare (ignore x)) <wordvector>)
(defmethod %bard-type-of ((x ccl::simple-unsigned-doubleword-vector))(declare (ignore x)) <wordvector>)

;;; ---------------------------------------------------------------------
;;; structures with Bard-specific representations
;;; ---------------------------------------------------------------------

(defclass %bard-base-structure% (%bard-structure%)
  ((structure-class :accessor %structure-class :initarg :structure-class)))

(defmethod print-object ((struct %bard-base-structure%)(s stream))
  (princ (class-name (%structure-class struct)) s))

;;; <actor>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <actor>
(defclass |<actor>| (%bard-structure-instance%)())

;;; the object used to represent the <actor> structure in Bard
(defparameter <actor> (make-instance '%bard-base-structure% :structure-class (find-class '|<actor>|)))

(defmethod %<actor>? (x)(declare (ignore x)) nil)
(defmethod %<actor>? ((x |<actor>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<actor>|))(declare (ignore x)) <actor>)

;;; <alist-table>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <alist-table>
(defclass |<alist-table>| (%bard-structure-instance%)())

;;; the object used to represent the <alist-table> structure in Bard
(defparameter <alist-table> (make-instance '%bard-base-structure% :structure-class (find-class '|<alist-table>|)))

(defmethod %<alist-table>? (x)(declare (ignore x)) nil)
(defmethod %<alist-table>? ((x |<alist-table>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<alist-table>|))(declare (ignore x)) <alist-table>)

;;; <bard>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <bard>
(defclass |<bard>| (%bard-structure-instance%)())

;;; the object used to represent the <bard> structure in Bard
(defparameter <bard> (make-instance '%bard-base-structure% :structure-class (find-class '|<bard>|)))

(defmethod %<bard>? (x)(declare (ignore x)) nil)
(defmethod %<bard>? ((x |<bard>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<bard>|))(declare (ignore x)) <bard>)

;;; <bit>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <bit>
(defclass |<bit>| (%bard-structure-instance%)())

;;; the object used to represent the <bit> structure in Bard
(defparameter <bit> (make-instance '%bard-base-structure% :structure-class (find-class '|<bit>|)))

(defmethod %<bit>? (x)(declare (ignore x)) nil)
(defmethod %<bit>? ((x |<bit>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<bit>|))(declare (ignore x)) <bit>)

;;; <class>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <class>
(defclass |<class>| (%bard-structure-instance%)
  ((name :accessor %class-name :initarg :name)))

(defmethod print-object ((class |<class>|)(s stream))
  (princ (%class-name class) s))

;;; the object used to represent the <class> structure in Bard
(defparameter <class> (make-instance '%bard-base-structure% :structure-class (find-class '|<class>|)))

(defmethod %<class>? (x)(declare (ignore x)) nil)
(defmethod %<class>? ((x |<class>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<class>|))(declare (ignore x)) <class>)

;;; <eof>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <eof>
(defclass |<eof>| (%bard-structure-instance%)())

;;; the object used to represent the <eof> structure in Bard
(defparameter <eof> (make-instance '%bard-base-structure% :structure-class (find-class '|<eof>|)))

(defmethod %<eof>? (x)(declare (ignore x)) nil)
(defmethod %<eof>? ((x |<eof>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<eof>|))(declare (ignore x)) <eof>)

;;; <false>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <false>
(defclass |<false>| (%bard-structure-instance%)())

;;; the object used to represent the <false> structure in Bard
(defparameter <false> (make-instance '%bard-base-structure% :structure-class (find-class '|<false>|)))

(defmethod %<false>? (x)(declare (ignore x)) nil)
(defmethod %<false>? ((x |<false>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<false>|))(declare (ignore x)) <false>)

;;; <function>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <function>
(defclass |<function>| (%bard-structure-instance%)())

;;; the object used to represent the <function> structure in Bard
(defparameter <function> (make-instance '%bard-base-structure% :structure-class (find-class '|<function>|)))

(defmethod %<function>? (x)(declare (ignore x)) nil)
(defmethod %<function>? ((x |<function>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<function>|))(declare (ignore x)) <function>)

;;; <getter>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <getter>
(defclass |<getter>| (%bard-structure-instance%)())

;;; the object used to represent the <getter> structure in Bard
(defparameter <getter> (make-instance '%bard-base-structure% :structure-class (find-class '|<getter>|)))

(defmethod %<getter>? (x)(declare (ignore x)) nil)
(defmethod %<getter>? ((x |<getter>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<getter>|))(declare (ignore x)) <getter>)

;;; <macro>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <macro>
(defclass |<macro>| (%bard-structure-instance%)())

;;; the object used to represent the <macro> structure in Bard
(defparameter <macro> (make-instance '%bard-base-structure% :structure-class (find-class '|<macro>|)))

(defmethod %<macro>? (x)(declare (ignore x)) nil)
(defmethod %<macro>? ((x |<macro>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<macro>|))(declare (ignore x)) <macro>)

;;; <method>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <method>
(defclass |<method>| (%bard-structure-instance%)())

;;; the object used to represent the <method> structure in Bard
(defparameter <method> (make-instance '%bard-base-structure% :structure-class (find-class '|<method>|)))

(defmethod %<method>? (x)(declare (ignore x)) nil)
(defmethod %<method>? ((x |<method>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<method>|))(declare (ignore x)) <method>)

;;; <setter>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <setter>
(defclass |<setter>| (%bard-structure-instance%)())

;;; the object used to represent the <setter> structure in Bard
(defparameter <setter> (make-instance '%bard-base-structure% :structure-class (find-class '|<setter>|)))

(defmethod %<setter>? (x)(declare (ignore x)) nil)
(defmethod %<setter>? ((x |<setter>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<setter>|))(declare (ignore x)) <setter>)

;;; <signed-byte>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <signed-byte>
(defclass |<signed-byte>| (%bard-structure-instance%)())

;;; the object used to represent the <signed-byte> structure in Bard
(defparameter <signed-byte> (make-instance '%bard-base-structure% :structure-class (find-class '|<signed-byte>|)))

(defmethod %<signed-byte>? (x)(declare (ignore x)) nil)
(defmethod %<signed-byte>? ((x |<signed-byte>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<signed-byte>|))(declare (ignore x)) <signed-byte>)

;;; <singleton>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <singleton>
(defclass |<singleton>| (%bard-structure-instance%)())

;;; the object used to represent the <singleton> structure in Bard
(defparameter <singleton> (make-instance '%bard-base-structure% :structure-class (find-class '|<singleton>|)))

(defmethod %<singleton>? (x)(declare (ignore x)) nil)
(defmethod %<singleton>? ((x |<singleton>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<singleton>|))(declare (ignore x)) <singleton>)

;;; <structure>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <structure>
(defclass |<structure>| (%bard-structure-instance%)())

;;; the object used to represent the <structure> structure in Bard
(defparameter <structure> (make-instance '%bard-base-structure% :structure-class (find-class '|<structure>|)))

(defmethod %<structure>? (x)(declare (ignore x)) nil)
(defmethod %<structure>? ((x |<structure>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<structure>|))(declare (ignore x)) <structure>)

;;; <true>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <true>
(defclass |<true>| (%bard-structure-instance%)())

;;; the object used to represent the <true> structure in Bard
(defparameter <true> (make-instance '%bard-base-structure% :structure-class (find-class '|<true>|)))

(defmethod %<true>? (x)(declare (ignore x)) nil)
(defmethod %<true>? ((x |<true>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<true>|))(declare (ignore x)) <true>)

;;; <undefined>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <undefined>
(defclass |<undefined>| (%bard-structure-instance%)())

;;; the object used to represent the <undefined> structure in Bard
(defparameter <undefined> (make-instance '%bard-base-structure% :structure-class (find-class '|<undefined>|)))

(defmethod %<undefined>? (x)(declare (ignore x)) nil)
(defmethod %<undefined>? ((x |<undefined>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<undefined>|))(declare (ignore x)) <undefined>)

;;; <unsigned-byte>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <unsigned-byte>
(defclass |<unsigned-byte>| (%bard-structure-instance%)())

;;; the object used to represent the <unsigned-byte> structure in Bard
(defparameter <unsigned-byte> (make-instance '%bard-base-structure% :structure-class (find-class '|<unsigned-byte>|)))

(defmethod %<unsigned-byte>? (x)(declare (ignore x)) nil)
(defmethod %<unsigned-byte>? ((x |<unsigned-byte>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<unsigned-byte>|))(declare (ignore x)) <unsigned-byte>)

;;; <url>
;;; ---------------------------------------------------------------------

;;; the class used to create instances of <url>
(defclass |<url>| (%bard-structure-instance%)())

;;; the object used to represent the <url> structure in Bard
(defparameter <url> (make-instance '%bard-base-structure% :structure-class (find-class '|<url>|)))

(defmethod %<url>? (x)(declare (ignore x)) nil)
(defmethod %<url>? ((x |<url>|))(declare (ignore x)) t)

(defmethod %bard-type-of ((x |<url>|))(declare (ignore x)) <url>)


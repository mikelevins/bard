;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               source-form.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This package exports functions to parse and manipulate
;;;;    Common Lisp sources as lisp forms (such as in macros).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-02-06 <PJB> Corrected the superclass of orakawbe-ll.
;;;;                     preqvars instanciated the wrong parameter class.
;;;;                     bodyvar poped the body parameter name.
;;;;    2006-05-25 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2006 - 2007
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(CL:IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.SOURCE-FORM"
  (:NICKNAMES "SOURCE-FORM")
  (:USE "COMMON-LISP")
  (:EXPORT
   ;; Parameter Classes:
   "PARAMETER" "ENVIRONMENT-PARAMETER" "WHOLE-PARAMETER"
   "REST-PARAMETER" "BODY-PARAMETER"
   "SPECIALIZED-PARAMETER" "AUXILIARY-PARAMETER"
   "OPTIONAL-PARAMETER" "GENERIC-OPTIONAL-PARAMETER"
   "KEYWORD-PARAMETER"  "GENERIC-KEYWORD-PARAMETER"
   ;; Parameter Methods:
   "PARAMETER-NAME" "PARAMETER-LABEL" #|"PARAMETER-HELP-LABEL"|#
   "PARAMETER-LAMBDA-LIST-KEYWORD"
   "PARAMETER-SPECIFIER" "PARAMETER-INDICATOR" "PARAMETER-INDICATOR-P"
   "PARAMETER-INITFORM" "PARAMETER-INITFORM-P" "PARAMETER-KEYWORD"
   "PARAMETER-KEYWORD-P" "ENSURE-PARAMETER-KEYWORD"
   "PARAMETER-SPECIALIZER" "PARAMETER-SPECIALIZER-P"
   ;; Lambda-List Classes:
   "LAMBDA-LIST" "ORDINARY-LAMBDA-LIST" "BOA-LAMBDA-LIST"
   "SPECIALIZED-LAMBDA-LIST" "MODIFY-MACRO-LAMBDA-LIST" "GENERIC-LAMBDA-LIST"
   "MACRO-LAMBDA-LIST" "TYPE-LAMBDA-LIST" "DESTRUCTURING-LAMBDA-LIST"
   "SETF-LAMBDA-LIST" "METHOD-COMBINATION-LAMBDA-LIST"
   ;; Lambda-List Methods:
   "ORIGINAL-LAMBDA-LIST" #|"LAMBDA-LIST-PARAMETERS"|#
   "LAMBDA-LIST-MANDATORY-PARAMETERS"  "LAMBDA-LIST-OPTIONAL-PARAMETERS"
   "LAMBDA-LIST-REST-PARAMETER" "LAMBDA-LIST-ALLOW-OTHER-KEYS-P" "LAMBDA-LIST-KEY-P"
   "LAMBDA-LIST-KEYWORD-PARAMETERS" "LAMBDA-LIST-ENVIRONMENT-PARAMETER"
   "LAMBDA-LIST-AUXILIARY-PARAMETERS" "LAMBDA-LIST-WHOLE-PARAMETER"
   "LAMBDA-LIST-ENVIRONMENT-PARAMETER" "LAMBDA-LIST-BODY-PARAMETER"
   "LAMBDA-LIST-KIND" "LAMBDA-LIST-ALLOWED-KEYWORDS"
   "LAMBDA-LIST-MANDATORY-PARAMETER-COUNT"
   "LAMBDA-LIST-OPTIONAL-PARAMETER-COUNT" "LAMBDA-LIST-REST-P"
   "LAMBDA-LIST-MANDATORY-PARAMETERS-P" "LAMBDA-LIST-OPTIONAL-PARAMETERS-P"
   "LAMBDA-LIST-REST-PARAMETER-P" "LAMBDA-LIST-AUXILIARY-PARAMETERS-P"
   "LAMBDA-LIST-WHOLE-PARAMETER-P" "LAMBDA-LIST-BODY-PARAMETER-P"
   "LAMBDA-LIST-ENVIRONMENT-PARAMETER-P"
   ;; Parsing lambda-lists:
   "PARSE-LAMBDA-LIST" "PARSE-ORIGINAL-LAMBDA-LIST"
   ;; Generating information from a lambda-list instance:
   "MAKE-HELP" "MAKE-ARGUMENT-LIST" "MAKE-ARGUMENT-LIST-FORM" "MAKE-LAMBDA-LIST"
   ;; Parsing sources:
   "EXTRACT-DOCUMENTATION" "EXTRACT-DECLARATIONS" "EXTRACT-BODY"
   "DECLARATIONS-HASH-TABLE"
   "EXTRACT-METHOD-QUALIFIERS"   "EXTRACT-METHOD-LAMBDA-LIST"
   "EXTRACT-METHOD-DDL"          "EXTRACT-METHOD-DOCUMENTATION"
   "EXTRACT-METHOD-DECLARATIONS" "EXTRACT-METHOD-BODY"
   ;; "DEFUN""DEFGENERIC""DEFMETHOD"
   ;; *CALL-STACK*" ;; not yet
   )
  (:shadow "READTABLE"
           "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
           "READ" "READ-PRESERVING-WHITESPACE"
           "READ-DELIMITED-LIST"
           "READ-FROM-STRING"
           "READTABLE-CASE" "READTABLEP"
           "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
           "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
           "SET-SYNTAX-FROM-CHAR"
           "WITH-STANDARD-IO-SYNTAX"
           "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
           "*READ-SUPPRESS*" "*READTABLE*")
  (:DOCUMENTATION "
    This package exports functions to parse and manipulate
    Common Lisp sources as lisp forms (such as in macros).

    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.SOURCE-FORM")



;; Not prudent:
;; #+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
;;           (import '(system::splice system::unquote system::backquote)))


;;;----------------------------------------
;;; Parameter specifications in lambda-lists
;;;----------------------------------------

;; Syntax of parameter specifications:
;;
;;    name
;; | (name [ specializer ])                       ; for specialized lambda-lists
;; | (name [ init-form [ indicator ]])            ; for &key &optional
;; | ((name keyword) [ init-form [ indicator ]])  ; for &key


(defmacro define-default-generic (name class default-value)
  `(defgeneric ,name (self)
     (:method ((self ,class)) (declare (ignore self)) ,default-value)))

;;;--------------------

(DEFGENERIC parameter-name-p (self))
(DEFGENERIC parse-parameter (self form))
(DEFGENERIC parse-parameter-name (self form))
(DEFGENERIC ensure-parameter-keyword (self))
(DEFGENERIC lambda-list-mandatory-parameter-count (self))
(DEFGENERIC lambda-list-optional-parameter-count (self))
(DEFGENERIC parse-optvars (self current slot lambda-list-keyword class))
(DEFGENERIC auxvars (self current))
(DEFGENERIC optvars (self current))
(DEFGENERIC goptvars (self current))
(DEFGENERIC parse-keyvars (self current class))
(DEFGENERIC keyvars (self current))
(DEFGENERIC gkeyvars (self current))
(DEFGENERIC parse-reqvars (self current class))
(DEFGENERIC reqvars (self current))
(DEFGENERIC sreqvars (self current))
(DEFGENERIC preqvars (self current))
(DEFGENERIC parse-original-lambda-list (self))
(DEFGENERIC make-help (self))
(DEFGENERIC make-argument-list (self))
(DEFGENERIC make-lambda-list (self))

;;;--------------------

(defclass parameter ()
  ((name :accessor parameter-name
         :initarg :name
         :type     symbol
         :documentation "The name of the parameter."))
  (:documentation "A generic parameter."))

(defmethod parameter-name-p ((self parameter))
  (slot-boundp self 'name))


(define-default-generic parameter-indicator     parameter nil)
(define-default-generic parameter-indicator-p   parameter nil)
(define-default-generic parameter-initform      parameter nil)
(define-default-generic parameter-initform-p    parameter nil)
(define-default-generic parameter-keyword       parameter nil)
(define-default-generic parameter-keyword-p     parameter nil)
(define-default-generic parameter-specializer   parameter nil)
(define-default-generic parameter-specializer-p parameter nil)


(defmethod parse-parameter-name ((self parameter) form)
  (if (symbolp form)
      (setf (parameter-name self) form)
      (error "Invalid parameter name: ~S" form))
  self)


(defmethod parse-parameter ((self parameter) form)
  (parse-parameter-name self form))

(defmethod print-object ((self parameter) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~A ~S"
            (parameter-lambda-list-keyword self)
            (parameter-specifier self))))

;;;--------------------

(defclass environment-parameter (parameter)
  ()
  (:documentation "An &ENVIRONMENT parameter."))

(defclass whole-parameter (parameter)
  ()
  (:documentation "A &WHOLE parameter."))

(defclass rest-parameter (parameter)
  ()
  (:documentation "A &REST parameter."))

(defclass body-parameter (parameter)
  ()
  (:documentation "A &BODY parameter."))


;;;--------------------

(defclass specialized-parameter (parameter)
  ((specializer :accessor parameter-specializer
                :initarg :specializer
                :type    (or symbol cons)
                :documentation "
   A specializer can be either NIL (no specializer),p
   a symbol denoting a class, or
   a cons (eql object) denoting an EQL specializer."))
  (:documentation "A specialized parameter."))

(defmethod parameter-specializer-p ((self specialized-parameter))
  (slot-boundp self 'specializer))

(defmethod parse-parameter ((self specialized-parameter) form)
  (etypecase form
    (symbol (call-next-method))
    (cons   (call-next-method self (first form))
            (when (cdr form)
              (setf (parameter-specializer self) (second form))
              (when (cddr form)
                (error "~A specification must be a ~
                        list of two elements at most, not ~S"
                       (parameter-label self) form)))))
  self)


;;;--------------------

(defclass parameter-with-initform ()
  ((initform   :accessor parameter-initform
               :initarg :initform
               :documentation "The initial form for the parameter."))
  (:documentation "A mixin for a parameter that may have an initform."))

(defmethod parameter-initform-p ((self parameter-with-initform))
  (slot-boundp self 'initform))

(defmethod parse-parameter ((self parameter-with-initform) form)
  (etypecase form
    (symbol (call-next-method))
    (cons   (call-next-method self (first form))
            (when  (cdr form)
              (setf (parameter-initform self) (second form)))))
  self)


;;;--------------------


(defclass auxiliary-parameter (parameter-with-initform parameter)
  ;; The order of the superclasses is important
  ;; to find the methods in the right order!
  ()
  (:documentation "An auxiliary parameter."))

(defmethod parse-parameter ((self auxiliary-parameter) form)
  (etypecase form
    (symbol (call-next-method))
    (cons   (call-next-method)
            (when (cddr form)
              (error "~A specification must be a ~
                      list of two elements at most, not ~S"
                     (parameter-label self) form))))
  self)


;;;--------------------

(defclass optional-parameter (parameter-with-initform parameter)
  ;; The order of the superclasses is important
  ;; to find the methods in the right order!
  ((indicator :accessor parameter-indicator
              :initarg :indicator
              :type symbol
              :documentation "NIL, or the name of the indicator parameter."))
  (:documentation "An optional parameter.
    Note that while auxiliary-parameter and optional-parameter have the
    same initform attribute, an optional-parameter is a different kind from
    an auxiliary-parameter, semantically."))

(defmethod parameter-initform-p ((self optional-parameter))
  (slot-boundp self 'initform))

(defmethod parameter-indicator-p ((self optional-parameter))
  (slot-boundp self 'indicator))

(defmethod parse-parameter ((self optional-parameter) form)
  (etypecase form
    (symbol (call-next-method))
    (cons   (call-next-method)
            (when (cddr form)
              (setf (parameter-indicator self) (third form))
              (when (cdddr form)
                (error "~A specification must be a ~
                        list of three elements at most, not ~S"
                       (parameter-label self) form)))))
  self)

;;;--------------------

(defclass generic-optional-parameter (parameter)
  ()
  (:documentation "An optional parameter in generic lambda-lists."))

(defmethod parse-parameter ((self generic-optional-parameter) form)
  (etypecase form
    (symbol (call-next-method))
    (cons   (call-next-method self (first form))
            (when (cdr form)
              (error "~A specification must be a ~
                        list of one element at most, not ~S"
                     (parameter-label self) form)))))


;;;--------------------

(defclass parameter-with-keyword ()
  ((keyword :accessor parameter-keyword
            :initarg :keyword
            :type    symbol
            :documentation "NIL, or the keyword specified for the parameter."))
  (:documentation "A mixin for keyword parameters."))

(defmethod parameter-keyword-p ((self parameter-with-keyword))
  (slot-boundp self 'keyword))

(defmethod parse-parameter-name ((self parameter-with-keyword) form)
  (etypecase form
    (symbol (call-next-method))
    (cons   (if (= 2 (length form))
                (progn
                  (call-next-method self (second form))
                  (setf (parameter-keyword self) (first form)))
                (error "~A specification must be a ~
                        list of two elements, not ~S"
                       (parameter-label self) form))))
  self)

(defmethod ensure-parameter-keyword ((self parameter-with-keyword))
  (if (parameter-keyword-p self)
      (parameter-keyword self)
      (INTERN (STRING (parameter-name self)) "KEYWORD")))

;;;--------------------

(defclass keyword-parameter (parameter-with-keyword optional-parameter)
  ;; The order of the superclasses is important
  ;; to find the methods in the right order!
  ()
  (:documentation "A keyword parameter."))


;;;--------------------

(defclass generic-keyword-parameter (parameter-with-keyword
                                     generic-optional-parameter)
  ;; The order of the superclasses is important
  ;; to find the methods in the right order!
  ()
  (:documentation "A generic keyword parameter."))



;;;--------------------

(defgeneric parameter-label (parameter)
  (:method ((self parameter))                  "A mandatory parameter")
  (:method ((self environment-parameter))      "An environment parameter")
  (:method ((self whole-parameter))            "A whole parameter")
  (:method ((self rest-parameter))             "A rest parameter")
  (:method ((self body-parameter))             "A body parameter")
  (:method ((self specialized-parameter))      "A specialized parameter")
  (:method ((self auxiliary-parameter))        "An auxiliary parameter")
  (:method ((self optional-parameter))         "An optional parameter")
  (:method ((self generic-optional-parameter)) "A generic optional parameter")
  (:method ((self keyword-parameter))          "A keyword parameter")
  (:method ((self generic-keyword-parameter))  "A generic keyword parameter"))

(defgeneric parameter-lambda-list-keyword (parameter)
  (:method ((self parameter))                  '&mandatory)
  (:method ((self environment-parameter))      '&environment)
  (:method ((self whole-parameter))            '&whole)
  (:method ((self rest-parameter))             '&rest)
  (:method ((self body-parameter))             '&body)
  (:method ((self specialized-parameter))      '&specialized)
  (:method ((self auxiliary-parameter))        '&aux)
  (:method ((self optional-parameter))         '&optional)
  (:method ((self generic-optional-parameter)) '&generic-optional)
  (:method ((self keyword-parameter))          '&key)
  (:method ((self generic-keyword-parameter))  '&generic-key))


(defgeneric parameter-specifier (parameter)
  (:method ((self parameter))
    (parameter-name self))
  (:method ((self specialized-parameter))
    (cons (parameter-name self)
          (when (parameter-specializer-p self)
            (list (parameter-specializer self)))))
  (:method ((self auxiliary-parameter))
    (if (parameter-initform-p self)
        (list (parameter-name self)  (parameter-initform self))
        (parameter-name self)))
  (:method ((self parameter-with-initform))
    (if (parameter-initform-p self)
        (cons (parameter-name self)
              (cons (parameter-initform self)
                    (when (parameter-indicator-p self)
                      (list (parameter-indicator self)))))
        (parameter-name self)))
  (:method ((self parameter-with-keyword))
    (if (or (parameter-keyword-p self) (parameter-initform-p self))
        (cons (if (parameter-keyword-p self)
                  (list (parameter-keyword self)  (parameter-name self))
                  (parameter-name self))
              (when  (parameter-initform-p self)
                (cons (parameter-initform self)
                      (when (parameter-indicator-p self)
                        (list (parameter-indicator self))))))
        (parameter-name self)))
  (:method ((self generic-keyword-parameter))
    (if (parameter-keyword-p self)
        (list (list (parameter-keyword self) (parameter-name self)))
        (parameter-name self))))


;;;--------------------

(defclass or-ll ()
  ((mandatories      :accessor lambda-list-mandatory-parameters
                     :initarg :mandatory-parameters
                     :initform '()
                     :type     list)
   (optionals        :accessor lambda-list-optional-parameters
                     :initarg :optional-parameters
                     :initform '()
                     :type     list)
   (rest             :accessor lambda-list-rest-parameter
                     :initarg :rest-parameter
                     :type     (or null rest-parameter)))
  (:documentation
   "This class and its subclasses are mixin declaring formally
the attributes for the various lambda-list classes.  Semantically,
some constraints may be different from one lambda-list to the other."))

(defgeneric lambda-list-mandatory-parameters-p (self)
  (:method ((self or-ll)) (not (not (lambda-list-mandatory-parameters self))))
  (:method ((self t))     nil))

(defgeneric lambda-list-optional-parameters-p (self)
  (:method ((self or-ll)) (not (not (lambda-list-optional-parameters self))))
  (:method ((self t))     nil))

(defgeneric lambda-list-rest-parameter-p (self)
  (:method ((self or-ll)) (slot-boundp self 'rest))
  (:method ((self t))     nil))




(define-default-generic lambda-list-allow-other-keys-p    or-ll nil)
(define-default-generic lambda-list-key-p                 or-ll nil)
(define-default-generic lambda-list-keyword-parameters    or-ll nil)
(define-default-generic lambda-list-environment-parameter or-ll nil)
(define-default-generic lambda-list-auxiliary-parameters  or-ll nil)
(define-default-generic lambda-list-whole-parameter       or-ll nil)
(define-default-generic lambda-list-body-parameter        or-ll nil)


(defclass orak-ll (or-ll)
  ((allow-other-keys-p :accessor lambda-list-allow-other-keys-p
                       :initarg :allow-other-keys-p
                       :initform nil
                       :type     boolean
                       :documentation  "Whether &ALLOW-OTHER-KEYS is present.")
   (key-p              :accessor lambda-list-key-p
                       :initarg :key-p
                       :initform nil
                       :type     boolean
                       :documentation "Whether &KEY is present.")
   ;; We can have &KEY &ALLOW-OTHER-KEYS without any keyword.
   (keys               :accessor lambda-list-keyword-parameters
                       :initarg :keyword-parameters
                       :initform '()
                       :type     list)))

(defgeneric lambda-list-keyword-parameters-p (self)
  (:method ((self or-ll)) (not (not (lambda-list-keyword-parameters self)))))



(defclass orake-ll (orak-ll)
  ((environment      :accessor lambda-list-environment-parameter
                     :initarg :environment-parameter
                     :type     environment-parameter)))

(defclass oraka-ll (orak-ll)
  ((aux              :accessor lambda-list-auxiliary-parameters
                     :initarg :auxiliary-parameters
                     :initform '()
                     :type     list)))

(defclass orakawb-ll (oraka-ll)
  ((whole            :accessor lambda-list-whole-parameter
                     :initarg :whole-parameter
                     :type     whole-parameter)
   (body             :accessor lambda-list-body-parameter
                     :initarg :body-parameter
                     :type     body-parameter)))

(defclass orakawbe-ll (orakawb-ll)
  ((environment      :accessor lambda-list-environment-parameter
                     :initarg :environment-parameter
                     :type     environment-parameter)))

(defgeneric lambda-list-auxiliary-parameters-p (self)
  (:method ((self oraka-ll)) (not (not (lambda-list-auxiliary-parameters self))))
  (:method ((self t))        nil))

(defgeneric lambda-list-whole-parameter-p (self)
  (:method ((self orakawb-ll)) (slot-boundp self 'whole))
  (:method ((self t))          nil))

(defgeneric lambda-list-body-parameter-p (self)
  (:method ((self orakawb-ll)) (slot-boundp self 'body))
  (:method ((self t))          nil))

(defgeneric lambda-list-environment-parameter-p (self)
  (:method ((self orakawbe-ll)) (slot-boundp self 'environment))
  (:method ((self orake-ll))    (slot-boundp self 'environment))
  (:method ((self t))           nil))



;;;----------------------------------------

(defclass lambda-list ()
  ((original   :accessor original-lambda-list
               :initarg :lambda-list
               :type     list)
   (parameters :accessor lambda-list-parameters
               :initarg :parameters
               :type     list
               :documentation "An ordered list of the parameters or destructuring-lambda-list instances."))
  (:documentation "An abstract lambda-list."))

(defclass ordinary-lambda-list           (lambda-list oraka-ll)    ())
(defclass boa-lambda-list                (lambda-list oraka-ll)    ())
(defclass specialized-lambda-list        (lambda-list oraka-ll)    ())
(defclass modify-macro-lambda-list       (lambda-list or-ll)       ())
(defclass generic-lambda-list            (lambda-list orak-ll)     ())
(defclass macro-lambda-list              (lambda-list orakawbe-ll) ())
(defclass type-lambda-list               (lambda-list orakawbe-ll) ())
(defclass destructuring-lambda-list      (lambda-list orakawb-ll)  ())
(defclass setf-lambda-list               (lambda-list orake-ll)    ())
(defclass method-combination-lambda-list (lambda-list orakaw-ll)   ())

(defgeneric lambda-list-kind (lambda-list)
  (:METHOD ((SELF ORDINARY-LAMBDA-LIST))           :ORDINARY)
  (:METHOD ((SELF BOA-LAMBDA-LIST))                :BOA)
  (:METHOD ((SELF SPECIALIZED-LAMBDA-LIST))        :SPECIALIZED)
  (:METHOD ((SELF MODIFY-MACRO-LAMBDA-LIST))       :MODIFY-MACRO)
  (:METHOD ((SELF GENERIC-LAMBDA-LIST))            :GENERIC)
  (:METHOD ((SELF MACRO-LAMBDA-LIST))              :MACRO)
  (:METHOD ((SELF TYPE-LAMBDA-LIST))               :TYPE)
  (:METHOD ((SELF DESTRUCTURING-LAMBDA-LIST))      :DESTRUCTURING)
  (:METHOD ((SELF SETF-LAMBDA-LIST))               :SETF)
  (:METHOD ((SELF METHOD-COMBINATION-LAMBDA-LIST)) :METHOD-COMBINATION))

(defgeneric lambda-list-allowed-keywords (lambda-list)
  (:METHOD ((SELF ORDINARY-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX))
  (:METHOD ((SELF BOA-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX))
  (:METHOD ((SELF SPECIALIZED-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX))
  (:METHOD ((SELF MODIFY-MACRO-LAMBDA-LIST)) '(&OPTIONAL &REST))
  (:METHOD ((SELF GENERIC-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY))
  (:METHOD ((SELF MACRO-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX &WHOLE &BODY &ENVIRONMENT))
  (:METHOD ((SELF TYPE-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX &WHOLE &BODY &ENVIRONMENT))
  (:METHOD ((SELF DESTRUCTURING-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX &WHOLE &BODY))
  (:METHOD ((SELF SETF-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &ENVIRONMENT))
  (:METHOD ((SELF METHOD-COMBINATION-LAMBDA-LIST))
    '(&OPTIONAL &REST &ALLOW-OTHER-KEYS &KEY &AUX &WHOLE)))



(defmethod lambda-list-mandatory-parameter-count ((self or-ll))
  "RETURN: The number of mandatory parameters."
  (length (lambda-list-mandatory-parameters self)))

(defmethod lambda-list-optional-parameter-count ((self or-ll))
  "RETURN: The number of optional parameters."
  (length (lambda-list-mandatory-parameters self)))

(defgeneric lambda-list-rest-p (self)
  (:documentation "RETURN: Whether &REST or &BODY parameters are present.")
  (:method ((self or-ll))      (lambda-list-rest-parameter-p self))
  (:method ((self orakawb-ll)) (or (lambda-list-rest-parameter-p self)
                                   (lambda-list-body-parameter-p self))))


;; auxvars  ::= [&aux {var | (var [init-form])}*]
;; optvars  ::= [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;; goptvars ::= [&optional {var | (var)}*]

(defmethod parse-optvars ((self or-ll) current slot lambda-list-keyword class)
  "
DO:     Parses optional parameters.
RETURN: The remaining tokens.
"
  (when (eq (car current) lambda-list-keyword)
    (pop current)
    (setf (slot-value self slot)
          (loop
             :while (and current (not (member (car current) lambda-list-keywords)))
             :collect (parse-parameter (make-instance class) (pop current)))))
  current)

(defmethod auxvars  ((self or-ll) current)
  (parse-optvars self current 'aux       '&aux      'auxiliary-parameter))
(defmethod optvars  ((self or-ll) current)
  (parse-optvars self current 'optionals '&optional 'optional-parameter))
(defmethod goptvars ((self or-ll) current)
  (parse-optvars self current 'optionals '&optional 'generic-optional-parameter))


;; keyvars  ::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*  [&allow-other-keys]]
;; gkeyvars ::= [&key {var | ({var | (keyword-name var)})}* [&allow-other-keys]])
(defmethod parse-keyvars ((self orak-ll) current class)
  "
DO:     Parses keywork parameters.
RETURN: The remaining tokens.
"
  (when (eq '&key (car current))
    (pop current)
    (setf (lambda-list-key-p self) t
          (lambda-list-keyword-parameters self)
          (loop
             :while (and current (not (member (car current) lambda-list-keywords)))
             :collect (parse-parameter (make-instance class) (pop current)))
          (lambda-list-allow-other-keys-p self)
          (and (eq '&allow-other-keys (car current)) (pop current) t)))
  current)

(defmethod keyvars  ((self orak-ll) current)
  (parse-keyvars self current 'keyword-parameter))
(defmethod gkeyvars ((self orak-ll) current)
  (parse-keyvars self current 'generic-keyword-parameter))


;; reqvars  ::= var*
;; sreqvars ::= {var | (var [specializer])}*
;; preqvars ::= {var | destructuring-lambda-list}*

(defmethod parse-reqvars ((self or-ll) current class)
  "
DO:     Parses required parameters.
RETURN: (values list-of-parameters following)
"
  (setf (lambda-list-mandatory-parameters self)
        (loop
           :while (and current (not (member (car current) lambda-list-keywords)))
           :collect (parse-parameter (make-instance class) (pop current))))
  current)

(defmethod reqvars  ((self or-ll) current)
  (parse-reqvars self current  'parameter))
(defmethod sreqvars ((self or-ll) current)
  (parse-reqvars self current  'specialized-parameter))


(defmethod preqvars ((self or-ll) current)
  "
DO:     Parses required parameters or patterns.
RETURN: (values list-of-parameters following)
"
  (setf (lambda-list-mandatory-parameters self)
        (loop
           :while (and current (not (member (car current) lambda-list-keywords)))
           :collect (if (consp (car current))
                        (parse-original-lambda-list
                         (make-instance 'destructuring-lambda-list
                           :lambda-list (pop current)))
                        (parse-parameter
                         (make-instance 'parameter)
                         (pop current)))))
  current)


;; bodyvar  ::= [{&rest | &body} var]
;; restvar  ::= [&rest var]
;; wholevar ::= [&whole var]
;; envvar   ::= [&environment var]

(defun bodyvar (self current)
  "
RETURN: (values parameter following)
"
  (flet ((check-duplicate (lambda-list-keyword)
           (when (lambda-list-rest-p self)
             (error "~:[&BODY~;&REST~] parameter already given before ~A in ~S"
                    (lambda-list-rest-parameter-p self)
                    lambda-list-keyword
                    (original-lambda-list self)))))
    (case (car current)
      ((&rest)
       (check-duplicate (pop current))
       (setf (lambda-list-rest-parameter self)
             (parse-parameter (make-instance 'rest-parameter) (pop current))))
      ((&body)
       (check-duplicate (pop current))
       (setf (lambda-list-body-parameter self)
             (parse-parameter (make-instance 'body-parameter) (pop current)))))
    current))


(defun parse-var (self current slot lambda-list-keyword class )
  "
RETURN: (values parameter following)
"
  (when (eq (car current) lambda-list-keyword)
    (pop current)
    (when (slot-boundp self slot)
      (error "~A parameter duplicated in ~S"
             lambda-list-keyword (original-lambda-list self)))
    (setf (slot-value self slot)
          (parse-parameter (make-instance class) (pop current))))
  current)

(defun restvar  (self current)
  (parse-var self current 'rest        '&rest        'rest-parameter))
(defun wholevar (self current)
  (parse-var self current 'whole       '&whole       'whole-parameter))
(defun envvar   (self current)
  (parse-var self current 'environment '&environment 'environment-parameter))



;; macro-lambda-list ::= (wholevar envvar preqvars envvar optvars envvar
;;                   bodyvar  envvar keyvars envvar auxvars envvar)
;;                | (wholevar envvar preqvars envvar optvars envvar .  var)
;;
;; destructuring-lambda-list ::= (wholevar preqvars optvars bodyvar keyvars auxvars)
;;                        | (wholevar preqvars optvars . var)
;;
;; type-lambda-list               ::= macro-lambda-list
;;
;;
;; ordinary-lambda-list           ::= (reqvars  optvars restvar keyvars auxvars)
;; boa-lambda-list                ::= ordinary-lambda-list
;; specialized-lambda-list        ::= (sreqvars optvars restvar keyvars auxvars)
;; generic-lambda-list            ::= (reqvars  goptvars restvar gkeyvars)
;; setf-lambda-list               ::= (reqvars optvars restvar keyvars envvar)
;; modify-macro-lambda-list       ::= (reqvars optvars restvar)
;; method-combination-lambda-list ::= (wholevar reqvars optvars restvar keyvars auxvars)


(defun parse-rest (self current syntax)
  (if (listp current)
      (dolist (fun syntax current)
        (setf current (funcall fun self current)))
      (restvar self (list '&rest current))))

(defun destructuring-rest (self current)
  (parse-rest self current  '(bodyvar keyvars auxvars)))

(defun macro-rest (self current)
  (parse-rest self current '(bodyvar envvar keyvars envvar auxvars envvar)))



(defgeneric lambda-list-syntax (self)
  (:method ((self ordinary-lambda-list))
    '(reqvars  optvars  restvar keyvars auxvars))
  (:method ((self boa-lambda-list))
    '(reqvars  optvars  restvar keyvars auxvars))
  (:method ((self specialized-lambda-list))
    '(sreqvars optvars  restvar keyvars auxvars))
  (:method ((self generic-lambda-list))
    '(reqvars  goptvars restvar gkeyvars))
  (:method ((self setf-lambda-list))
    '(reqvars  optvars restvar keyvars envvar))
  (:method ((self modify-macro-lambda-list))
    '(reqvars  optvars restvar))
  (:method ((self method-combination-lambda-list))
    '(wholevar reqvars optvars restvar keyvars auxvars))
  (:method ((self macro-lambda-list))
    '(wholevar envvar preqvars envvar optvars envvar macro-rest))
  (:method ((self type-lambda-list))
    '(wholevar envvar preqvars envvar optvars envvar macro-rest))
  (:method ((self destructuring-lambda-list))
    '(wholevar preqvars optvars destructuring-rest)))


(defmethod parse-original-lambda-list ((self lambda-list))
  (let ((current (original-lambda-list self)))
    (dolist (fun (lambda-list-syntax self))
      (setf current (funcall fun self current)))
    (when current
      (error "Syntax error in ~(~A~) at: ~S~%in ~S"
             (class-name (class-of self)) current (original-lambda-list self)))
    self))


(defun parse-lambda-list (lambda-list &optional (kind :ordinary))
  "
DO:      Parse a lambda-list of the specified kind.
KIND:    (MEMBER :ORDINARY :BOA :SPECIALIZED :MODIFY-MACRO :GENERIC
                 :MACRO :TYPE :DESTRUCTURING :SETF :METHOD-COMBINATION)
RETURN:  A lambda-list instance.
"
  (parse-original-lambda-list
   (make-instance
       (or (cdr (assoc
                 kind
                 '((:ordinary           . ordinary-lambda-list)
                   (:boa                . boa-lambda-list)
                   (:specialized        . specialized-lambda-list)
                   (:modify-macro       . modify-macro-lambda-list)
                   (:generic            . generic-lambda-list)
                   (:macro              . macro-lambda-list)
                   (:type               . type-lambda-list)
                   (:destructuring      . destructuring-lambda-list)
                   (:setf               . setf-lambda-list)
                   (:method-combination . method-combination-lambda-list))))
           (error "Invalid lambda-list kind ~S" kind))
     :lambda-list lambda-list)))

;;------------------------------------------------------------------------

(defgeneric parameter-help-label (self)
  (:method ((self parameter))
    (format nil "~A" (parameter-name self)))
  (:method ((self optional-parameter))
    (format nil "[~A]" (parameter-name self)))
  (:method ((self rest-parameter))
    (format nil "~A..." (parameter-name self)))
  (:method ((self body-parameter))
    (format nil "~A..." (parameter-name self)))
  (:method ((self keyword-parameter))
    (format nil "~A" (ensure-parameter-keyword self))))


(defmethod make-help ((self lambda-list))
  "
RETURN: A list describing the lambda-list for the user. Each item is a cons:
        (lambda-list-keyword . description) where
        - the lambda-list-keyword is either
          :mandatory, :optional, :rest, :body, :key,  or :allow-other-keys.
        - the description is a string indicating the name of the parameter,
          and whether it's optional '[n]' or takes several arguments 'n...'.
"
  (append
   ;; mandatory:
   (mapcar (lambda (par) (cons :mandatory (parameter-help-label par)))
           (lambda-list-mandatory-parameters self))
   ;; optional:
   (mapcar (lambda (par) (cons :optional  (parameter-help-label par)))
           (lambda-list-optional-parameters self))
   (when (lambda-list-rest-parameter-p self)
     (list (cons :rest (parameter-help-label (lambda-list-rest-parameter self)))))
   (when (lambda-list-body-parameter-p self)
     (list (cons :body (parameter-help-label (lambda-list-body-parameter self)))))
   ;; keywords:
   (mapcar (lambda (par) (cons :key (parameter-help-label par)))
           (lambda-list-keyword-parameters self))
   (when (lambda-list-allow-other-keys-p self)
     (list (cons :allow-other-keys "(other keys allowed)")))))


(defmethod make-argument-list ((self lambda-list))
  "
RETURN: A list of arguments taken from the parameters usable with apply
        to call a function with the same lambda-list.
"
  (let ((rest (lambda-list-rest-p self)))
    (append
     (mapcar (function parameter-name) (lambda-list-mandatory-parameters self))
     (mapcar (function parameter-name) (lambda-list-optional-parameters  self))
     (when (lambda-list-key-p self)
       (mapcan (lambda (par) (list (ensure-parameter-keyword par)
                              (parameter-name par)))
               (lambda-list-keyword-parameters  self)))
     (list (if rest  (parameter-name rest) 'nil)))))




;;;; MAKE-ARGUMENT-LIST-FORM
;; +------+--------+-----+---------+
;; | rest | k-wo-i | aok | all-opt |
;; +------+--------+-----+---------+
;; |  no  |   no   |  no | <=> there is some keyword
;; |  no  |   no   | yes | <=> there is some keyword ; we can't know the other keywords!
;; |  no  |   yes  |  no | yes
;; |  no  |   yes  | yes | yes ; we can't know the other keywords!
;; |  yes |   no   |  no | <=> there is some keyword <=> (not (null rest))
;; |  yes |   no   | yes | <=> there is some keyword <=> (not (null rest))
;; |  yes |   yes  |  no | yes
;; |  yes |   yes  | yes | yes
;; +------+--------+-----+---------+

(defgeneric make-argument-list-form (lambda-list))
(defmethod make-argument-list-form ((self lambda-list))
  "
RETURN: A form that will build a list of arguments passing the same arguments
        given to lambda-list, to be passed to APPLY.
NOTE:   If optional or key arguments have an indicator,
        then they're not passed unless necessary or the indicator is true.
BUG:    We don't handle MACRO-LAMBDA-LISTs nor DESTRUCTURING-LAMBDA-LISTs, etc.
"
  (flet ((genopt ()
           (loop
              :with result = '()
              :with pars = (reverse (lambda-list-optional-parameters self))
              :for par = (pop pars)
              :while (and par (parameter-indicator-p par))
              :do (push `(when ,(parameter-indicator par)
                           (list ,(parameter-name par))) result) 
              :finally (return
                         `(,@(when (or par pars)
                                   `((list ,@(nreverse
                                              (mapcar
                                               (function parameter-name)
                                               (if par
                                                   (cons par pars)
                                                   pars))))))
                             ,@result)))))
    (let* ((rest
            (cond
              ((lambda-list-rest-parameter-p self) (lambda-list-rest-parameter self))
              ((lambda-list-body-parameter-p self) (lambda-list-body-parameter self))))
           (form
             `(append
               ,@(if (not (every (function parameter-indicator-p)
                                 (lambda-list-keyword-parameters self)))
                     ;; If some keyword parameter has no indicator,
                     ;; we will be forced to pass it again as argument,
                     ;; therefore we must pass all optional argumentst too.
                     `( (list ,@(mapcar (function parameter-name)
                                        (lambda-list-mandatory-parameters self))
                              ,@(mapcar (function parameter-name)
                                        (lambda-list-optional-parameters self))))

                     `( (list ,@(mapcar (function parameter-name)
                                        (lambda-list-mandatory-parameters self)))
                        ,@(if (not (or rest (lambda-list-keyword-parameters self)))
                              (genopt)
                              `((if
                                 ,(if rest
                                      (parameter-name rest)
                                      `(or
                                        ,@(mapcar
                                           (function parameter-indicator)
                                           (lambda-list-keyword-parameters self))))
                                 (list ,@(mapcar
                                          (function parameter-name)
                                          (lambda-list-optional-parameters self)))
                                 ,(let ((subforms (genopt)))
                                       (cond
                                         ((null subforms) '())
                                         ((cdr subforms) `(append ,@subforms))
                                         (t (car subforms)))))))))
               ,@(if rest
                     ;; When we have a rest (or body) parameter, we don't need
                     ;; to generate the keyword parameters, since they're 
                     ;; covered by the rest. We just append the rest to the
                     ;;  list of arguments.
                     `(,(parameter-name rest))
                     ;; Without a rest (or body) parameter, we need to pass
                     ;; the keyword arguments.
                     (mapcar (lambda (parameter)
                               (if (parameter-indicator-p parameter)
                                   ;; If we have an indicator parameter,
                                   ;; we pass the keyword argument
                                   ;; only when we got it.
                                   `(when ,(parameter-indicator parameter)
                                      (list
                                       ,(ensure-parameter-keyword parameter)
                                       ,(parameter-name parameter)))
                                   ;; otherwise we pass the keyword argument
                                   ;; unconditionnaly:
                                   `(list ,(ensure-parameter-keyword parameter)
                                          ,(parameter-name parameter))))
                             (lambda-list-keyword-parameters  self))))))
      (if (= 2 (length form))
          (second form)
          form))))



(defmethod make-lambda-list ((self lambda-list))
  "
RETURN:     A newly rebuilt lambda-list s-expr.
"
  (append
   (when (lambda-list-whole-parameter-p self)
     (list '&whole
           (parameter-specifier (lambda-list-whole-parameter self))))
   (when (lambda-list-environment-parameter-p self)
     (list '&environment
           (parameter-specifier (lambda-list-environment-parameter self))))
   (mapcar (function parameter-specifier) (lambda-list-mandatory-parameters self))
   (when (lambda-list-optional-parameters self)
     (cons '&optional
           (mapcar (function parameter-specifier)
                   (lambda-list-optional-parameters self))))
   (when (lambda-list-body-parameter-p self)
     (list '&body (parameter-specifier (lambda-list-body-parameter self))))
   (when (lambda-list-rest-parameter-p self)
     (list '&rest (parameter-specifier (lambda-list-rest-parameter self))))
   (when (lambda-list-key-p self)
     '(&key))
   (when (lambda-list-keyword-parameters self)
     (mapcar (function parameter-specifier)
             (lambda-list-keyword-parameters self)))
   (when (lambda-list-allow-other-keys-p self)
     '(&allow-other-keys))
   (when (lambda-list-auxiliary-parameters self)
     (cons '&aux (mapcar (function parameter-specifier)
                         (lambda-list-auxiliary-parameters self))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (defmacro m (&environment env &whole whole
;;              ((a b) (c (d e)) &optional (o t op))
;;              e f &body g &key k1 k2)
;;   (print (list env whole a b c d e o op e f g k1 k2)) nil)
;;
;; (m ((1 2) (3 (4 5))) 6 7  :k1 (print c) :k2 (print d))
;;
;; (#(NIL NIL)
;;   (M ((1 2) (3 (4 5))) 6 7 :K1 (PRINT C) :K2 (PRINT D))
;;   1 2 3 4 6 T NIL 6 7
;;   (:K1 (PRINT C) :K2 (PRINT D))
;;   (PRINT C)
;;   (PRINT D))


;; (make-help-from-split-lambda-list
;;  (split-lambda-list-on-keywords
;;   '(m1 m2 m3 &optional o1 o2 o3 &rest r1 &key k1 k2 k3 &aux a1 a2 a3
;;     &allow-other-keys)
;;   :ordinary))
;;'(m1 m2 m3 &optional o1 o2 o3 &rest r1 &key k1 k2 k3 &aux a1 a2 a3  &allow-other-keys)




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-documentation (body)
    "
RETURN: The documentation string found in BODY, or NIL if none is present.

3.4.11 Syntactic Interaction of Documentation Strings and Declarations

In a number of situations, a documentation string can appear amidst a series of
declare expressions prior to a series of forms.

In that case, if a string S appears where a documentation string is permissible
and is not followed by either a declare expression or a form then S is taken to
be a form; otherwise, S is taken as a documentation string. The consequences
are unspecified if more than one such documentation string is present.
"
    (loop
       :for (item . rest) :on body
       :while (and (consp item) (eq 'declare (first item)))
       :finally (return (and (stringp item) rest item))))


  (defun extract-declarations (body)
    "
RETURN: The list of declaration forms.
"
    (loop
       :with seen-doc = nil
       :for item :in body
       :while (or (and (not seen-doc) (stringp item))
                  (and (consp item) (eq 'declare (car item))))
       :when  (and (not seen-doc) (stringp item)) :do (setf seen-doc t)
       :when  (and (consp item) (eq 'declare (car item))) :collect item))


  (defun declarations-hash-table (declarations)
    ;; Todo: add some knowledge on how declarations merge.
    (loop
       :with table = (make-hash-table)
       :for decl :in declarations
       :do (loop
              :for (key . value) :in (rest decl)
              :do (push value (gethash key table '())))
       :finally (return table)))


  (defun extract-body (body)
    (loop
       :with seen-doc = nil
       :for (item . rest) :on body
       :while (or (and (not seen-doc) (stringp item))
                  (and (consp item) (eq 'declare (car item))))
       :when (and (not seen-doc) (stringp item)) :do (setf seen-doc t)
       :finally (return (cons item rest)))))


(defun extract-method-qualifiers (method-stuff)
  (loop
     :for item :in method-stuff
     :until (listp item)
     :collect item))

(defun extract-method-lambda-list (method-stuff)
  (loop
     :for item :in method-stuff
     :until (listp item)
     :finally (return item)))

(defun extract-method-ddl (method-stuff)
  (loop
     :for (item . body) :in method-stuff
     :until (listp item)
     :finally (return body)))

(defun extract-method-documentation (method-stuff)
  (extract-documentation (extract-method-ddl method-stuff)))

(defun extract-method-declarations (method-stuff)
  (extract-declarations (extract-method-ddl method-stuff)))

(defun extract-method-body (method-stuff)
  (extract-body (extract-method-ddl method-stuff)))



;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (shadow '(DEFUN DEFGENERIC DEFMETHOD)))
;; 
;; 
;; (defparameter *call-stack* '())
;; 
;; 
;; (cl:defmacro defun (name args &body body)
;;   (let ((lambda-list (parse-lambda-list args :ordinary))
;;         (docu (extract-documentation body))
;;         (decl (extract-declarations  body))
;;         (body (extract-body          body)))
;;     `(cl:defun ,name ,args
;;        ,@(when docu (list docu))
;;        ,@decl
;;        (push (list ',name ,@(make-argument-list lambda-list)) *call-stack*)
;;        (multiple-value-prog1 (progn ,@body)
;;          (pop *call-stack*)))))
;; 
;; 
;; (cl:defmacro defmethod (name &rest stuff)
;;   (let* ((qualifiers (extract-method-qualifiers stuff))
;;          (args       (extract-method-lambda-list     stuff))
;;          (lambda-list     (parse-lambda-list args :specialized))
;;          (docu       (extract-method-documentation stuff))
;;          (decl       (extract-method-declarations  stuff))
;;          (body       (extract-method-body          stuff)))
;;     `(cl:defmethod
;;          ,name ,@qualifiers ,args
;;          ,@(when docu (list docu))
;;          ,@decl
;;          (push (list ',name ,@(make-argument-list lambda-list)) *call-stack*)
;;          (multiple-value-prog1 (progn ,@body)
;;            (pop *call-stack*)))))
;; 
;; (cl:defmacro defgeneric (name args &rest options-and-methods)
;;   `(cl:defgeneric ,name ,args
;;      ,@(mapcar
;;         (lambda (item)
;;           (if (and (consp item) (eq :method (car item)))
;;               (let* ((stuff      (rest item))
;;                      (qualifiers (extract-method-qualifiers stuff))
;;                      (args       (extract-method-lambda-list     stuff))
;;                      (lambda-list     (parse-lambda-list args :specialized))
;;                      (docu       (extract-method-documentation stuff))
;;                      (decl       (extract-method-declarations  stuff))
;;                      (body       (extract-method-body          stuff)))
;;                 `(:method ,@qualifiers ,args
;;                           ,@(when docu (list docu))
;;                           ,@decl
;;                           (push (list ',name ,@(make-argument-list lambda-list))
;;                                 *call-stack*)
;;                           (multiple-value-prog1 (progn ,@body)
;;                             (pop *call-stack*))))
;;               item))
;;         options-and-methods)))

;;;; The End ;;;;

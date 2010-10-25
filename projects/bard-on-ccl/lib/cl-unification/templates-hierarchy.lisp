;;;; -*- Mode: Lisp -*-

;;;; templates-hierarchy.lisp --

(in-package "CL.EXT.DACF.UNIFICATION") ; DACF = Data And Control Flow.

;;; Templates.
;;; Essentially we extend the type specifier language.
;;; The interesting thing is that we need to specify how a "match"
;;; between a template and a CL object is performed.

;;; A template is one of the following
;;;
;;;    <template> ::= <logical variable>
;;;               |   <structure template>
;;;               |   <instance template>
;;;               |   <destructuring template>
;;;               |   <vector template>
;;;               |   <sequence template>
;;;               |   <array template>
;;;               |   <type template>
;;;               |   <lisp object>
;;;

;;; Destructuring Template Lambda List (as per standard CL terminology)
;;;
;;;     <destructuring template lambda list> ::= <a "destructuring LL" with <template> in lieu of <var>>

;;; Templates for Structures and Instances
;;;
;;;	<structure template> ::= (<class designator> <structure-slot-spec>*)
;;;
;;;     <instance template>  ::= (<class designator> <slot-spec>*)
;;;
;;; where
;;;
;;;     <structure-slot-spec> ::= <reader-name> <template>
;;;     <instance-slot-spec>  ::= <accessor-name> <template>
;;;                           |   (slot-value <slot-name>) <template>

;;; Template for Sequences
;;;
;;;     <sequence template>    ::= (<container type> . <destructuring template lambda list>)
;;;                            |   (subseq <from> <to> . <destructuring template lambda list>)
;;;     <container type>       ::= list | cons | vector | array

;;; Templates for Vectors and Arrays.
;;;
;;;     <vector template> ::= (vector . <destructuring template lambda list>)
;;;
;;;     <array template>  ::= (array <shape template>)
;;;                       |   (<array CL type specifier> <shape template>)
;;;                       |   (array (['*' | <element type>] [<dimension spec>]]) <shape template>)
;;;                       |   (aref <index template> <template>)
;;;     <shape template>  ::= <destructuring template lambda list>
;;;                       |   <sequence template>
;;;                       |   (<shape template>)

;;; Templates for LIST and CONS
;;;     <list template>   ::= (list . <destructuring template lambda list>)
;;;     <cons template>   ::= (cons <template> <template>)

;;; A regular list or cons acts as a list (hence as a sequence) template, or a cons template.

(define-condition unification-template-error (simple-error)
  ())

;;; Templates are introduced by the reader macro #T(...)

(defclass template ()
  ((spec :accessor template-spec :type (or symbol cons) :initarg :spec))
  (:default-initargs :spec nil))

(defgeneric template-p (x)
  (:method ((x template)) t)
  (:method ((x t)) nil))


(defclass type-template (template) ())

(defgeneric type-template-p (x)
  (:method ((x type-template)) t)
  (:method ((x t)) nil))


(defgeneric type-template-type-spec (x)
  (:method ((x type-template))
   (let ((spec (template-spec x)))
     (if spec
         (first spec)
         'null))))



(defclass nil-template (type-template) ()) ; This is the point where we break the type hierarchy.

(defgeneric nil-template-p (x)
  (:method ((x nil-template)) t)
  (:method ((x t)) nil))


(defclass expression-template (template) ())

(defgeneric expression-template-p (x)
  (:method ((x expression-template)) t)
  (:method ((x t)) nil))


(defmethod print-object ((template template) (stream stream))
  (format stream "#T~S" (template-spec template)))


(defclass sequence-template (type-template) ())

(defgeneric sequence-template-p (x)
  (:method ((x sequence-template)) t)
  (:method ((x t)) nil))


(defclass list-template (sequence-template) ())

(defgeneric list-template-p (x)
  (:method ((x list-template)) t)
  (:method ((x t)) nil))


(defclass lambda-template (list-template expression-template) ())

(defgeneric lambda-template-p (x)
  (:method ((x lambda-template)) t)
  (:method ((x t)) nil))



(defclass array-template (type-template) ())

(defgeneric array-template-p (x)
  (:method ((x array-template)) t)
  (:method ((x t)) nil))


(defclass vector-template (sequence-template array-template) ())

(defgeneric vector-template-p (x)
  (:method ((x vector-template)) t)
  (:method ((x t)) nil))


(defclass string-template (vector-template) ())

(defgeneric string-template-p (x)
  (:method ((x string-template)) t)
  (:method ((x t)) nil))




(defclass symbol-template (type-template) ())

(defgeneric symbol-template-p (x)
  (:method ((x symbol-template)) t)
  (:method ((x t)) nil))


(defclass number-template (type-template) ())

(defgeneric number-template-p (x)
  (:method ((x number-template)) t)
  (:method ((x t)) nil))


(defclass structure-object-template (type-template) ())

(defgeneric structure-object-template-p (x)
  (:method ((x structure-object-template)) t)
  (:method ((x t)) nil))


(defclass standard-object-template (type-template) ())

(defgeneric standard-object-template-p (x)
  (:method ((x standard-object-template)) t)
  (:method ((x t)) nil))


;;; Expression Templates.

(defclass subseq-template (expression-template) ())

(defgeneric subseq-template-p (x)
  (:method ((x subseq-template)) t)
  (:method ((x t)) nil))



(defclass element-template (expression-template) ())

(defgeneric element-template-p (x)
  (:method ((x element-template)) t)
  (:method ((x t)) nil))


(defclass elt-template (element-template) ())

(defgeneric elt-template-p (x)
  (:method ((x elt-template)) t)
  (:method ((x t)) nil))


(defclass aref-template (element-template) ())

(defgeneric aref-template-p (x)
  (:method ((x aref-template)) t)
  (:method ((x t)) nil))


(defclass nth-template (element-template) ())

(defgeneric nth-template-p (x)
  (:method ((x nth-template)) t)
  (:method ((x t)) nil))


(defclass nthcdr-template (element-template) ())

(defgeneric nthcdr-template-p (x)
  (:method ((x nthcdr-template)) t)
  (:method ((x t)) nil))


(defgeneric make-template (kind spec))


;;; Setting up the reader macro.

;;; 20080711 MA:
;;; Reverted to the old version with MAKE-LOAD-FORM added.  Template
;;; objects are created at read-time.

(defun |sharp-T-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((spec (read stream t nil t)))
    (typecase spec
      (null (make-template nil spec))
      (cons (make-template (first spec) spec))
      (t    (make-template spec spec)))))

(defmethod make-load-form ((x template) &optional env)
  (make-load-form-saving-slots x :environment env))


#||
;;; Version with more 'macro-like' behavior. The previous version
;;; created an object at read-time, which may cause problems with
;;; MAKE-LOAD-FORMs, constant-ness etc etc.
;;;
;;; 20080713 MA
;;; Removed because it was not working well with nested templates.
;;; Reverted to the original one plus MAKE-LOAD-FORM.

(defun |sharp-T-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((spec (read stream t nil t)))
    (typecase spec
      (null `(make-template nil ',spec))
      (cons `(make-template ',(first spec) ',spec))
      (t    `(make-template ',spec ',spec)))
    ))
||#

(eval-when (:load-toplevel :execute)
  (set-dispatch-macro-character #\# #\T '|sharp-T-reader|))


#|| Useless with the read time templates and MAKE-LOAD-FORM.

(defun rewrite-template-spec (spec)
  "Rewrites a template specification.
The rewriting simply makes sure that sub-templates are created as needed.
The result is either the SPEC itself or an appropriate call to LIST."

  (typecase spec
    (atom `',spec)
    (cons (destructuring-bind (head &rest tail)
              spec
            (case head
              (quote spec)
              (make-template `(make-template ,(first tail)
                                             ,(rewrite-template-spec (second (second tail)))))
              (t `(list ',head ,@(mapcar #'rewrite-template-spec tail)))
              )))
    (t `',spec)))

||#
  


(defmethod make-template ((kind null) (spec symbol))
  (assert (null spec) (spec) "MAKE-TEMPLATE called erroneously with ~S and ~S." kind spec)
  (make-instance 'nil-template :spec spec))

(defmethod make-template ((kind symbol) (spec symbol))
  (make-instance 'symbol-template :spec spec))

(defmethod make-template ((kind (eql 'symbol)) (spec cons))
  (make-instance 'symbol-template :spec spec))

(defmethod make-template ((kind symbol) (spec cons))
  (cond ((subtypep kind 'number)
         (make-instance 'number-template :spec spec))
        ((subtypep kind 'structure-object)
         (make-instance 'structure-object-template :spec spec))
        ((subtypep kind 'standard-object)
         (make-instance 'standard-object-template :spec spec))
        (t
         (error 'unification-template-error
                :format-control "Unknown template specifier ~S."
                :format-arguments (list kind)))
        ))

(defmethod make-template ((kind cons) (spec cons))
  (cond ((subtypep kind 'number)
         (make-instance 'number-template :spec spec))
        ((subtypep kind 'string)
         (make-instance 'string-template :spec spec))
        ((subtypep kind 'vector)
         (make-instance 'vector-template :spec spec))
        ((subtypep kind 'array)
         (make-instance 'array-template :spec spec))
        (t
         (error 'unification-template-error
                :format-control "Unknown template specifier ~S."
                :format-arguments (list kind)))
        ))

(defmethod make-template ((kind number) (spec number))
  (assert (= kind spec))
  (make-instance 'number-template :spec spec))

(defmethod make-template ((kind (eql 'sequence)) (spec cons))
  (make-instance 'sequence-template :spec spec))

(defmethod make-template ((kind (eql 'list)) (spec cons))
  (make-instance 'list-template :spec spec))

(defmethod make-template ((kind (eql 'lambda)) (spec cons))
  (make-instance 'lambda-template :spec spec))

(defmethod make-template ((kind (eql 'vector)) (spec cons))
  (make-instance 'vector-template :spec spec))

(defmethod make-template ((kind (eql 'string)) (spec cons))
  (make-instance 'string-template :spec spec))

(defmethod make-template ((kind (eql 'array)) (spec cons))
  (make-instance 'array-template :spec spec))


(defmethod make-template ((kind (eql 'subseq)) (spec cons))
  (make-instance 'subseq-template :spec spec))

(defmethod make-template ((kind (eql 'elt)) (spec cons))
  (make-instance 'elt-template :spec spec))

(defmethod make-template ((kind (eql 'aref)) (spec cons))
  (make-instance 'aref-template :spec spec))

(defmethod make-template ((kind (eql 'nth)) (spec cons))
  (make-instance 'nth-template :spec spec))

(defmethod make-template ((kind (eql 'nthcdr)) (spec cons))
  (make-instance 'nthcdr-template :spec spec))



;;;;===========================================================================
;;;; Implementation.

;;; Symbol Templates.
;;; Specification is
;;;
;;;    (symbol <symbol>)

(defun symbol-template-symbol (x)
  (declare (type symbol-template x))
  (assert (symbol-template-p x) (x) "Non symbol template ~S." x)
  (let ((spec (template-spec x)))
    (cond ((symbolp spec) spec)
          ((consp spec) (second spec)))))


;;; Number template
;;; Specification is
;;;
;;;	(<number type> <number>)
;;; or
;;;
;;;	<number>

(defun number-template-number (x)
  (declare (type number-template x))
  (assert (number-template-p x) (x) "Non number template ~S." x)
  (let ((spec (template-spec x)))
    (etypecase spec
      (symbol spec)
      (number spec)
      (cons (second spec)))))


(defun number-template-numeric-type (x)
  (declare (type number-template x))
  (let ((n (number-template-number x)))
    (if (numberp n)
        (type-of n)
        (first (template-spec x)))))

(defun number-template-numeric-class (x)
  (declare (type number-template x))
  (let ((n (number-template-number x)))
    (if (numberp n)
        (class-of n)
        (find-class (first (template-spec x))))))




;;; Sequence Templates.
;;; Specification is
;;;
;;;	(<sequence subtype> . <destructuring template lambda list>)
;;; or
;;;     (subseq <from> <to> . <destructuring template lambda list>)

(defun sequence-template-lambda-list (x)
  (declare (type sequence-template x))
  (assert (sequence-template-p x) (x) "Non sequence template ~S." x)
  (rest (template-spec x)))


;;; Vector Templates.
;;; Specification is
;;;
;;;	(<vector type specifier> . <destructuring template lambda list>)

(defun vector-template-element-type (x)
  (declare (type vector-template x))
  (assert (vector-template-p x) (x) "Non vector template ~S." x)
  (let ((spec (type-template-type-spec x)))
    (if (consp spec)
        (destructuring-bind (vector-kwd &optional (element-type '*) size)
            spec
          (declare (ignore vector-kwd size))
          element-type)
        '*)))


(defun vector-template-size (x)
  (declare (type vector-template x))
  (assert (vector-template-p x) (x) "Non vector template ~S." x)
  (let ((spec (type-template-type-spec x)))
    (if (consp spec)
        (destructuring-bind (vector-kwd &optional element-type (size '*))
            spec
          (declare (ignore vector-kwd element-type))
          size)
        '*)))


;;; Array Templates.
;;; Specification is
;;;
;;;	(array (['*' | <element type>] [<dimension spec>])  <shape template>)
;;; or
;;;	(<array type specifier> <shape template>)


(defun array-template-shape-template (x)
  (declare (type array-template x))
  (assert (array-template-p x) (x) "Non array template ~S." x)
  (let ((t-spec (template-spec x)))
    (if (= 2 (list-length t-spec))
        (second t-spec)
        (third t-spec))))


(defun array-template-element-type (x)
  (declare (type array-template x))
  (assert (array-template-p x) (x) "Non array template ~S." x)
  (let ((type-spec (type-template-type-spec x)))
    (if (consp type-spec)
        (destructuring-bind (array-kwd &optional (element-type '*) dimension-spec)
            type-spec
          (declare (ignore array-kwd dimension-spec))
          element-type)
        '*)))


(defun array-template-dimensions (x)
  (declare (type array-template x))
  (assert (array-template-p x) (x) "Non array template ~S." x)
  (let ((type-spec (type-template-type-spec x)))
    (if (consp type-spec)
        (destructuring-bind (array-kwd &optional element-type (dimension-spec '*))
            type-spec
          (declare (ignore array-kwd element-type))
          dimension-spec)
        '*)))



;;; Structure and Standard Object Templates.

(defun structure-object-template-class (x)
  (and (structure-object-template-p x)
       (first (template-spec x))))

(defun structure-object-template-slots (x)
  (and (structure-object-template-p x)
       (rest (template-spec x))))


(defun standard-object-template-class (x)
  (and (standard-object-template-p x)
       (first (template-spec x))))

(defun standard-object-template-slots (x)
  (and (standard-object-template-p x)
       (rest (template-spec x))))


;;;---------------------------------------------------------------------------
;;; Expression Templates.


;;; AREF Templates.

(defun aref-template-indexes (x)
  (declare (type aref-template x))
  (assert (aref-template-p x) (x) "Non aref template ~S." x)
  (let ((spec (template-spec x)))
    (second spec)))


(defun aref-template-element (x)
  (declare (type aref-template x))
  (assert (aref-template-p x) (x) "Non array template ~S." x)
  (let ((spec (template-spec x)))
    (third spec)))




;;;===========================================================================
;;; Template variables.
;;; Let's walk down a template.
;;; Note that there is an asymmetry here: I admit some containers to have
;;; variables inside, but I do not search instances of a class for variables.
;;; This is an asymmetry that would be way too hard to fix without more
;;; introspective power (which is available in the MOP, but not standard.)

(defgeneric collect-template-vars (template))

(defmethod collect-template-vars ((template template))
  (let ((spec (template-spec template)))
    (nconc (collect-template-vars (car spec))
           (collect-template-vars (cdr spec)))))

(defmethod collect-template-vars ((template symbol-template))
  (let ((template (symbol-template-symbol template)))
    (when (and (variablep template) (not (variable-any-p template)))
      (list template))))


(defmethod collect-template-vars ((template number-template))
  (let ((template (number-template-number template)))
    (etypecase template
      (number ())
      (symbol (cond ((and (variablep template) (not (variable-any-p template)))
                     (list template))
                    ((and (boundp template)
                          (numberp (symbol-value template)))
                     ;; This handles cases like #T(number pi)
                     ;; It may be too broad, but for the time being it seems ok.
                     nil)
                    (t
                     (error "Invalid number template ~S." template)))))))


(defmethod collect-template-vars ((template symbol))
  (when (and (variablep template) (not (variable-any-p template)))
    (list template)))

(defmethod collect-template-vars ((template null))
  ())

(defmethod collect-template-vars ((template cons))
  (nconc (collect-template-vars (car template))
         (collect-template-vars (cdr template))))

(defmethod collect-template-vars ((template string))
  ())


(defmethod collect-template-vars ((template vector))
  (loop for e across template
        nconc (collect-template-vars e)))


(defmethod collect-template-vars ((template array))
  (loop for i below (array-total-size template)
        nconc (collect-template-vars (row-major-aref template i))))


(defmethod collect-template-vars ((template t))
  ())

;;; end of file -- templates.lisp --

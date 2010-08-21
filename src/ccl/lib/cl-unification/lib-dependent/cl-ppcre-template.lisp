;;;; -*- Mode: Lisp -*-

;;;; cl-ppcre-template.lisp --
;;;; REGEXP template dependent on CL-PPCRE.

(in-package "CL.EXT.DACF.UNIFICATION") ; DACF = Data And Control Flow.


;;;; REGEXP Templates.
;;;; Another extension of the type specifier language.

;;;; A template can also be
;;;;
;;;;    <template> ::= #| templates from template-hierarchy.lisp |#
;;;;               |   <regexp template>
;;;;
;;;; Hairier REGEXP template spec syntax:
;;;;
;;;; (regexp|regular-expression) <REGEXP> &optional <unification vars> &rest <keys>
;;;;
;;;; where
;;;;
;;;; <REGEXP> ::= <a CL-PPCRE regexp string or tree>
;;;; <unification vars> ::= '(' <variable>* ')'
;;;; <keys> ::= <CL-PPCRE (constant) keys to be passed to CL-PPCRE:CREATE-SCANNER>

(defclass regular-expression-template (string-template)
  ((scanner :reader scanner)
   (regexp :reader regular-expression)
   (vars :reader variables
         :reader registers
         :type list)
   )
  (:documentation "The Regular Expression Template.

A template for matching strings using regular expressions.
The actual matching is done thankes to the CL-PPCRE library.")
  )



(defgeneric regular-expression-template-p (x)
  (:method ((x regular-expression-template)) t)
  (:method ((x t)) nil))


(defmethod make-template ((kind (eql 'regexp)) (spec cons))
  (make-instance 'regular-expression-template :spec spec))

(defmethod make-template ((kind (eql 'regular-expression)) (spec cons))
  (make-template 'regexp spec))


(defmethod initialize-instance :after ((re-t regular-expression-template) &key)
  (destructuring-bind (re-kwd regexp &optional vars &rest keys)
      (template-spec re-t)
    (declare (ignore re-kwd))
    (multiple-value-bind (scanner reg-names)
        (let ((cl-ppcre:*allow-named-registers* t))
          (apply #'cl-ppcre:create-scanner regexp keys))
      (declare (ignorable reg-names))
      (setf (slot-value re-t 'scanner)
            scanner

            (slot-value re-t 'regexp)
            regexp

            (slot-value re-t 'vars)
            vars ; Maybe will merge with REG-NAMES...
            )
      )))

#|
(defmethod initialize-instance :after ((re-t regular-expression-template) &key)
  ;; FIX: handling of CL-PPCRE:CREATE-SCANNER keywords.  This can be
  ;; done by using the "harier" syntax of SPEC (see above).
  (destructuring-bind (re-kwd regexp &optional vars &rest keys)
      (template-spec re-t)
    (declare (ignore re-kwd)
             (ignorable regexp vars keys))
    (multiple-value-bind (scanner reg-names)
        (let ((cl-ppcre:*allow-named-registers* t))
          (cl-ppcre:create-scanner (second (template-spec re-t))))
      (declare (ignorable reg-names))
      (setf (slot-value re-t 'scanner)
            scanner

            (slot-value re-t 'regexp)
            (second (template-spec re-t)) ; For the time being just stored and
                                          ; used for debugging.
            )
      )))
|#

;;;;---------------------------------------------------------------------------
;;;; Implementation.

;;; Unification.

(defmethod unify ((ret1 regular-expression-template)
                  (ret2 regular-expression-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (if (eq ret1 ret2)
      env
      ;; I could UNIFY the result of the CL-PPCRE:PARSE-STRINGs.
      (error 'unification-failure
             :format-control "Do not know how unify the two ~
                              regular-expression templates: ~S and ~S."
             :format-arguments (list ret1 ret2))))


(defmethod unify ((re-t regular-expression-template) (s string)
                  &optional (env (make-empty-environment))
                  &key
                  (start 0)
                  end
                  &allow-other-keys)
  (declare (type (integer 0 #.most-positive-fixnum) start)
           (type (or null (integer 0 #.most-positive-fixnum)) end))
  (let ((end (or end (length s))))
    (declare (type (integer 0 #.most-positive-fixnum) end))

    (multiple-value-bind (matched-p strings)
	(cl-ppcre:scan-to-strings (scanner re-t) s :start start :end end)
      (unless matched-p
        (error 'unification-failure
               :format-control "String ~S cannot be matched against ~
                                regular expression ~S."
               :format-arguments (list s
                                       (regular-expression re-t))))
      (let ((vars (variables re-t)))
        (if (null vars)
            env
            (loop for r-string of-type string across strings
                  for v in vars
                  for result-env = (var-unify v r-string env)
                  then (var-unify v r-string result-env)
                  finally (return result-env))))
      )))


(defmethod unify ((s string) (re-t regular-expression-template)
                  &optional (env (make-empty-environment))
                  &key (start 0) end &allow-other-keys)
  (unify re-t s env :start start :end end))
  

;;;; end of file -- cl-ppcre-template.lisp --

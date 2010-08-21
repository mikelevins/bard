;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocols.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       support for Bard protocol objects
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

(defclass signature ()
  ((name :accessor signature-name :initarg :name)
   (parameters :accessor signature-parameters :initarg :parameters)
   (return-signature :accessor signature-return-signature :initarg :return-signature)))

(defmethod print-object ((sig signature) (str stream))
  (format str "[(~A ~{~A~}) => ~{~A~}]"
          (signature-name sig)(signature-parameters sig)(signature-return-signature sig)))

(defmethod signature-description-name ((sig list))
  (cl:first sig))

(defmethod signature-description-parameters ((sig list))
  (cl:second sig))

(defmethod signature-description-return-signature ((sig list))
  (cl:third sig))

(defun make-signature (sig)
  (make-instance 'signature 
                 :name (intern-for-name (signature-description-name sig))
                 :parameters (mapcar 'intern-for-name (signature-description-parameters sig))
                 :return-signature (mapcar 'intern-for-name (signature-description-return-signature sig))))

(defparameter $the-categories-table (map))
(defparameter $the-protocol-table (map))

(defun list-categories ()
  (keys $the-categories-table))

(defun assert-category! (cat)
  (or (get $the-categories-table cat nil)
      (setf $the-categories-table 
            (merge $the-categories-table 
                   (map cat nil)))))

(defclass protocol ()
  ((name :accessor protocol-name :initarg :name)
   (signatures :accessor protocol-signatures :initarg :signatures)))

(defmethod define-protocol ((name symbol) &rest signatures)
  (let ((categories (remove-duplicates (apply 'cl:append 
                                              (mapcar (lambda (s) (cl:append (elt s 1) (elt s 2))) 
                                                      signatures))))
        (p (map name (mapcar (lambda (s) (make-signature s)) signatures))))
    (dolist (c categories) (assert-category! c))
    (setf $the-protocol-table (merge $the-protocol-table p))))


#|

(describe (make-signature  '(|empty?| (|<Sequence>|) (|<Boolean>|))))
(define-protocol '|Sequence|
    '(|empty?| (|<Sequence>|) (|<Boolean>|))
  '(|first| (|<Sequence>|) (|<Value>|))
  '(|rest| (|<Sequence>|) (|<Value>|)))

|#
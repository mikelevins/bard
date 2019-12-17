;;;; ***********************************************************************
;;;;
;;;; Name:          types-tuples.scm
;;;; Project:       Bard
;;;; Purpose:       representing Bard tuple types
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; tuple structs
;;; =====================================================================

(define tags:$bard-tuple (%next-bard-type-number))
(define tags:$bard-tuple-instance (%next-bard-type-number))

(define (make-tuple name slot-specs)
  (let* ((tag tags:$bard-tuple)
         (slot-count (getf slot-count: slot-specs default: 0))
         (slot-type (getf slot-type: slot-specs default: Anything)))
    (make-tuple-struct name tag slot-count slot-type)))

;;; instance constructor

(define (initialize-tuple tuple-instance . initargs)
  (let* (
         (vals (getf values: initargs default: #f))
         (slots (tuple-instance-slots tuple-instance))
         (slot-count (vector-length slots)))
    (if vals
        (if (= slot-count (length vals))
            (let loop ((i 0))
              (if (< i slot-count)
                  (begin
                    (vector-set! slots i (list-ref vals i))
                    (loop (+ i 1)))))
            (error (str "Wrong number of values for a tuple type. Expected "
                        slot-count " but found " (length vals)))))
    tuple-instance))

;;; TODO: slot templates contain type information.
;;;       currently it's not used, but later
;;;       we can enforce it

(define (instantiate-tuple struct initargs)
  (let* ((slot-count (tuple-struct-slot-count struct))
         (default (getf default: initargs default: '()))
         (slots (make-vector slot-count default))
         (instance (make-tuple-instance struct slots)))
    (apply initialize-tuple instance initargs)))

;;; instance accessors
;;; TODO: add support for marking slots immutable

(define (tuple-ref tuple-instance index)
  (vector-ref (tuple-instance-slots tuple-instance) index))

(define (tuple-set! tuple-instance index val)
  (vector-set! (tuple-instance-slots tuple-instance) index val))

(define (tuple-put tuple-instance index val)
  (if (and (integer? index)
           (< -1 index (vector-length (tuple-instance-slots tuple-instance))))
      ;;; the index is in the range of this tuple type's indexes so we
      ;;; can make a new instsance of the tuple
      (let ((new-slots (vector-copy (tuple-instance-slots tuple-instance))))
        (vector-set! new-slots index val)
        (make-tuple-instance (instance-struct tuple-instance) new-slots))
      ;;; the index is outside the range of this tuple type's indexes;
      ;;; we can't represent the result with the same tuple type so we
      ;;; make a table instead
      (let* ((invals (vector->list (tuple-instance-slots tuple-instance)))
             (indexes (iota (length invals)))
             (slots (cons (cons index val)
                          (zip indexes invals))))
        (%make-alist-table slots))))






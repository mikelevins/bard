;;;; ***********************************************************************
;;;;
;;;; Name:          types-alist-table.scm
;;;; Project:       Bard
;;;; Purpose:       struct <alist-table>
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

;;; =====================================================================
;;; base structs
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <alist-table>
;;; ----------------------------------------------------------------------

(define tags:$bard-alist-table (%next-bard-type-number))
(define <alist-table> (make-base-struct '<alist-table> tags:$bard-alist-table))


;;; constructor

(define (%make-alist-table slots-alist)
  (let ((slots (map (lambda (s)(cons (car s)(cdr s)))
                    slots-alist)))
    (make-alist-table-instance <alist-table> slots)))

;;; accessors

(define alist-table-slots alist-table-instance-slots)
(define set-alist-table-slots! alist-table-instance-slots-set!)

(define (alist-table-get table key)
  (let* ((slots (alist-table-instance-slots table))
         (slot (assoc key slots)))
    (if slot
        (cdr slot)
        '())))

(define (alist-table-keys table)
  (let loop ((slots (alist-table-instance-slots table))
             (keys '()))
    (if (null? slots)
        (reverse keys)
        (let ((slot (car slots))
              (more (cdr slots)))
          (if (member (car slot) keys)
              (loop more keys)
              (loop more (cons (car slot) keys)))))))

(define (alist-table-put table key val)
  (make-alist-table-instance <alist-table> 
                             (cons (cons key val)
                                   (alist-table-instance-slots table))))

(define (alist-table-vals table)
  (let loop ((slots (alist-table-instance-slots table))
             (keys '())
             (vals '()))
    (if (null? slots)
        (reverse vals)
        (let ((slot (car slots))
              (more (cdr slots)))
          (if (member (car slot) keys)
              (loop more keys vals)
              (loop more
                    (cons (car slot) keys)
                    (cons (cdr slot) vals)))))))

(define (%table->list t)(alist-table-slots t))

(define (%merge-alist-tables t1 t2)
  (%make-alist-table
   (merge-alists (alist-table-instance-slots t1)
                 (alist-table-instance-slots t2))))

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define (%alist-table . args)
  (%make-alist-table (plist->alist args)))

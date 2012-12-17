;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          link.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard linker
;;;;                converts object code to executable code
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%map-labels code)
  (let ((len (vector-length code)))
    (let loop ((i 0)
               (label-map '()))
      (if (< i len)
          (let ((ins (vector-ref code i)))
            (if (symbol? ins)
                (loop (+ i 1) (cons (cons ins i) label-map))
                (loop (+ i 1) label-map)))
          label-map))))

(define (%label-index e label-map)
  (let ((found-entry (assoc e label-map)))
    (and found-entry (cdr found-entry))))

(define (%link-instruction! instruction #!optional (label-map '()))
  ;; resolve labels
  (for-each
   (lambda (i)
     (let* ((e (list-ref instruction i))
            (label-index (%label-index e label-map)))
       (if label-index ;; always make the jump point just past the label
           (set-nth-car! instruction i (+ 1 label-index)))))
   (iota (length instruction)))
  ;; link ops
  (set-car! instruction (%opname->opfn (car instruction)))
  instruction)

(define (%link-instructions! code label-map)
  (for-each
   (lambda (i)
     (let ((ins (vector-ref code i)))
       (if (pair? ins)
           (vector-set! code i (%link-instruction! ins label-map)))))
   (iota (vector-length code))))

(define (%link code)
  (let* ((linked-code (vector-copy code))
         (label-map (%map-labels linked-code)))
    (%link-instructions! linked-code label-map)
    code))


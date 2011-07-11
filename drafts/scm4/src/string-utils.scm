;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          string-utils.scm
;;;; Project:       bard
;;;; Purpose:       general string utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (string-position item str test)
  (let ((chars (string->list str)))
    (let loop ((its chars)
               (i 0))
      (if (null? its)
          #f
          (if (test item (car its))
              i
              (loop (cdr its)
                    (+ i 1)))))))

(define (string-every? pred str)
  (let* ((slist (string->list str)))
    (every? pred slist)))

(define (substring-position sub str test)
  (let ((sublen (string-length sub))
        (strlen (string-length str))
        (chartest (cond
                   ((eqv? test string=?) char=?)
                   ((eqv? test string-ci=?) char-ci=?)
                   (else (lambda (c1 c2)(test (string c1)(string c2)))))))
    (cond
     ((> sublen strlen) #f)
     ((= sublen strlen) (if (test sub str)
                            0
                            #f))
     (else (let ((max-index (- strlen sublen)))
             (let loop ((i 0))
               (if (<= i max-index)
                   (if (chartest (string-ref sub 0)
                                 (string-ref str i))
                       (if (test sub (substring str i (+ i sublen)))
                           i
                           (loop (+ i 1)))
                       (loop (+ i 1)))
                   #f)))))))

;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          list-utils.scm
;;;; Project:       bard
;;;; Purpose:       general list utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (plist->alist plist)
  (let loop ((plist plist)
             (result '()))
    (if (null? plist)
        (reverse result)
        (if (null? (cdr plist))
            (error "Malformed plist" plist)
            (loop (cddr plist)
                  (cons (cons (car plist)
                              (cadr plist))
                        result))))))

(define (every? pred ls)
  (if (null? ls)
      #t
      (if (pred (car ls))
          (every? pred (cdr ls))
          #f)))


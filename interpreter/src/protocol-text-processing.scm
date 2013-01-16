;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-text-processing.scm
;;;; Project:       Bard
;;;; Purpose:       system utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")

;;; ---------------------------------------------------------------------
;;; join-text
;;; ---------------------------------------------------------------------

(define (%bard-join-text cupola strs)
  (if (null? strs)
      ""
      (apply string-append 
             (cdr (apply append (map (lambda (s)(list cupola s))
                                     strs))))))

(define bard:join-text (make-function debug-name: 'join-text
                                      signatures: (list (signature (Text List) #f (Text)))))

(%add-primitive-method! bard:join-text
                        (list <string> <pair>)
                        %bard-join-text
                        debug-name: 'join-text)

;;; ---------------------------------------------------------------------
;;; split
;;; ---------------------------------------------------------------------

(define (%bard-split-string str ch)
  (let ((len (string-length str)))
    (let loop ((i 0)
               (last-found #f)
               (chunks '()))
      (if (< i len)
          (let ((foundch (string-ref str i)))
            (if (char=? foundch ch)
                (if last-found
                    (let ((chunk (substring str (+ 1 last-found) i)))
                      (loop (+ i 1) i (cons chunk chunks)))
                    (let ((chunk (substring str 0 i)))
                      (loop (+ i 1) i (cons chunk chunks))))
                (loop (+ i 1) last-found chunks)))
          (if last-found
              (let ((chunk (substring str (+ 1 last-found) i)))
                (reverse (cons chunk chunks)))
              (let ((chunk (substring str 0 i)))
                (reverse (cons chunk chunks))))))))

(define bard:split-text (make-function debug-name: 'split-text
                                       signatures: (list (signature (Text Character) #f (List)))))

(%add-primitive-method! bard:split-text
                        (list <string>  <character>)
                        %bard-split-string
                        debug-name: 'split-text)


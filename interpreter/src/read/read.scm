;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read.scm
;;;; Project:       Bard
;;;; Purpose:       bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; gambit prerequisutes
;;;---------------------------------------------------------------------

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

(##define-macro (macro-peek-next-char-or-eof re) ;; possibly returns EOF
 `(macro-peek-char (macro-readenv-port ,re)))

(##define-macro (macro-read-next-char-or-eof re) ;; possibly returns EOF
 `(macro-read-char (macro-readenv-port ,re)))

;;;---------------------------------------------------------------------
;;; the Bard readtable
;;;---------------------------------------------------------------------

(define (bard:read-character re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip backslash
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let ((c (macro-read-next-char-or-eof re))) ;; read char after backslash
      (cond ((not (char? c))(error "incomplete form, EOF reached"))
            ((char-alphabetic? c)
             (let* ((name (##build-delimited-string re c 1))
                    (not-hex (lambda ()
                               (let ((x (assoc name
                                               (macro-readtable-named-char-table
                                                (macro-readenv-readtable re)))))
                                 (if x
                                     (macro-readenv-wrap re (cdr x))
                                     (if (= 1 (string-length name))
                                         (macro-readenv-wrap re (string-ref name 0))
                                         (error "unknown character name" name)))))))
               (if (and (= 6 (string-length name))
                        (char=? (string-ref name 0) #\u)
                        (char=? (string-ref name 1) #\+))
                   (let ((n (string->number (substring name 2 6) 16)))
                     (if n
                         (macro-readenv-wrap re (integer->char n))
                         (not-hex)))
                   (not-hex))))
            (else
             (macro-readenv-wrap re c))))))

(define (bard:read-number/keyword/symbol re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let ((obj (##build-delimited-number/keyword/symbol re c #t)))
      (cond 
       ((eqv? obj 'undefined) 
        (macro-readenv-wrap re (bard:undefined)))
       ((eqv? obj 'nothing)
        (macro-readenv-wrap re '()))
       ((eqv? obj 'true)
        (macro-readenv-wrap re #t))
       ((eqv? obj 'false)
        (macro-readenv-wrap re #f))
       ((symbol? obj)
        (macro-readenv-wrap re obj))
       ((keyword? obj)
        (macro-readenv-wrap re obj))
       ((integer? obj)
        (macro-readenv-wrap re obj))
       ((flonum? obj)
        (macro-readenv-wrap re obj))
       ((##ratnum? obj)
        (macro-readenv-wrap re obj))
       (else (error "Unrecognized atom" c))))))

(define $constituent-chars
  (apply append
         (list
          (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (string->list "1234567890")
          (string->list "~!@$%^&*_-+=|:<>?/"))))

(define (bard:read-escaped-string re c)
  (##read-escaped-string re c))

(define (bard:make-readtable)
  (let ((rt (##make-standard-readtable)))
    (for-each (lambda (ch)(##readtable-char-class-set! rt ch #f bard:read-number/keyword/symbol))
              $constituent-chars)
    (macro-readtable-keywords-allowed?-set! rt #t)
    (##readtable-char-class-set! rt #\\ #t bard:read-character)
    (##readtable-char-class-set! rt #\" #t bard:read-escaped-string)
    (macro-readtable-bracket-keyword-set! rt 'bard:list)
    (macro-readtable-brace-keyword-set! rt 'bard:frame)
    rt))

(define +bard-readtable+ (bard:make-readtable))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------

(define (bard:read #!optional (port (current-input-port)))
  (let ((original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port +bard-readtable+))
        (lambda ()(read port))
        (lambda ()(input-port-readtable-set! port original-readtable)))))

(define (bard:read-from-string s)
  (call-with-input-string s
                          (lambda (in)
                            (bard:read in))))

;;; (bard:read-from-string "")
;;; (show (bard:read-from-string "undefined"))
;;; (show (bard:read-from-string "nothing"))
;;; (show (bard:read-from-string "true"))
;;; (show (bard:read-from-string "false"))
;;; (show (bard:read-from-string "5"))
;;; (show (bard:read-from-string "5.4"))
;;; (show (bard:read-from-string "5/4"))
;;; (show (bard:read-from-string "888888888"))
;;; (show (bard:read-from-string "\\C"))
;;; (show (bard:read-from-string "\\space"))
;;; (show (bard:read-from-string "\\u+0041"))
;;; (show (bard:read-from-string "\"Fred and Barney\""))
;;; (show (bard:read-from-string "Fred"))
;;; (show (bard:read-from-string "name:"))
;;; (show (bard:read-from-string "(0 1 2 3)"))
;;; (show (bard:read-from-string "[0 1 2 3]"))
;;; (show (bard:read-from-string "{0 1 2 3}"))

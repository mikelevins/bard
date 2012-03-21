;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.scm
;;;; Project:       bard
;;;; Purpose:       convert source text to abstract syntax trees
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; gambit reader prerequisites
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

(define (bard:read-quote re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'quote value: (<nothing>:nothing)))))

(define (bard:read-comma re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'comma value: (<nothing>:nothing)))))

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
                                     (macro-readenv-wrap re 
                                                         (<syntax-atom>:make syntax-type: 'character
                                                                             value: (cdr x)))
                                     (if (= 1 (string-length name))
                                         (macro-readenv-wrap re 
                                                             (<syntax-atom>:make syntax-type: 'character
                                                                                 value: (string-ref name 0)))
                                         (error "unknown character name" name)))))))
               (if (and (= 6 (string-length name))
                        (char=? (string-ref name 0) #\u)
                        (char=? (string-ref name 1) #\+))
                   (let ((n (string->number (substring name 2 6) 16)))
                     (if n
                         (macro-readenv-wrap re 
                                             (<syntax-atom>:make syntax-type: 'character
                                                                 value: (integer->char n)))
                         (not-hex)))
                   (not-hex))))
            (else
             (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'character
                                                        value: c)))))))

(define (bard:read-number/keyword/symbol re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let ((obj (##build-delimited-number/keyword/symbol re c #t)))
      (cond 
       ((eqv? obj 'undefined) 
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'undefined value: (<nothing>:nothing))))
       ((eqv? obj 'nothing)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'nothing value: (<nothing>:nothing))))
       ((eqv? obj 'true)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'boolean value: (<boolean>:true))))
       ((eqv? obj 'false)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'boolean value: (<boolean>:false))))
       ((symbol? obj)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'name value: obj)))
       ((integer? obj)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'integer value: obj)))
       ((flonum? obj)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'float value: obj)))
       ((##ratnum? obj)
        (macro-readenv-wrap re (<syntax-atom>:make syntax-type: 'ratio value: obj)))
       (else (error "Unrecognized atom" c))))))

(define $constituent-chars
  (apply append
         (list
          (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (string->list "1234567890")
          (string->list "~!@$%^&*_-+=|:<>?/"))))

(define (bard:read-escaped-string re c)
  (let ((s (##read-escaped-string re c)))
    (<syntax-atom>:make syntax-type: 'text value: s)))

(define (bard:make-readtable)
  (let ((rt (##make-standard-readtable)))
    (for-each (lambda (ch)(##readtable-char-class-set! rt ch #f bard:read-number/keyword/symbol))
              $constituent-chars)
    (macro-readtable-keywords-allowed?-set! rt #f)
    (##readtable-char-class-set! rt #\' #t bard:read-quote)
    (##readtable-char-class-set! rt #\, #t bard:read-comma)
    (##readtable-char-class-set! rt #\\ #t bard:read-character)
    (##readtable-char-class-set! rt #\" #t bard:read-escaped-string)
    (macro-readtable-paren-keyword-set! rt 'application)
    (macro-readtable-bracket-keyword-set! rt 'sequence)
    (macro-readtable-brace-keyword-set! rt 'frame)
    rt))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------

(define (bard:read port)
  (let ((original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port (bard:make-readtable)))
        (lambda ()(let ((s (read port)))
                    (cond
                     ((<eof>:eof? s)(<syntax-atom>:make syntax-type: 'eof value: (<nothing>:nothing)))
                     ((list? s)(let ((tag (car s))
                                     (s (cdr s)))
                                 (cond
                                  ((eq? 'application tag)
                                   (<syntax-sequence>:make-sequence s syntax-type: 'application))
                                  ((eq? 'sequence tag) 
                                   (<syntax-sequence>:make-sequence s syntax-type: 'sequence))
                                  ((eq? 'frame tag)
                                   (<syntax-sequence>:make-sequence s syntax-type: 'frame))
                                  (else (error "Unrecognized syntax" s)))))
                     (else s))))
        (lambda ()(input-port-readtable-set! port original-readtable)))))




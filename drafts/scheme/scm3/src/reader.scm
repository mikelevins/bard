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

(define bard:quote (list 'bard:quote))

(define (bard:read-quote re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (macro-readenv-wrap re bard:quote)))

(define bard:comma (list 'bard:comma))

(define (bard:read-comma re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (macro-readenv-wrap re bard:comma)))

(define (bard:read-character re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip backslash
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let ((c (macro-read-next-char-or-eof re))) ;; read char after backslash
      (cond ((not (char? c))
             (error "incomplete form, EOF reached"))
            ((char-alphabetic? c)
             (let ((name (##build-delimited-string re c 1)))
               (define (not-hex)
                 (let ((x (assoc name
                                 (macro-readtable-named-char-table
                                  (macro-readenv-readtable re)))))
                   (if x
                       (macro-readenv-wrap re (bard:%syntax-character (cdr x)))
                       (if (= 1 (string-length name))
                           (macro-readenv-wrap re (bard:%syntax-character (string-ref name 0)))
                           (error "unknown character name" name)))))
               (if (and (= 6 (string-length name))
                        (char=? (string-ref name 0) #\u)
                        (char=? (string-ref name 1) #\+))
                   (let ((n (string->number (substring name 2 6) 16)))
                     (if n
                         (macro-readenv-wrap re (bard:%syntax-character (integer->char n)))
                         (not-hex)))
                   (not-hex))))
            (else
             (macro-readenv-wrap re (bard:%syntax-character c)))))))

(define (bard:read-number/keyword/symbol re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip "c"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let ((obj (##build-delimited-number/keyword/symbol re c #t)))
      (cond 
       ((eqv? obj 'undefined) (macro-readenv-wrap re (bard:%syntax-undefined)))
       ((eqv? obj 'nothing) (macro-readenv-wrap re (bard:%syntax-nothing)))
       ((eqv? obj 'true) (macro-readenv-wrap re (bard:%syntax-true)))
       ((eqv? obj 'false) (macro-readenv-wrap re (bard:%syntax-false)))
       ((symbol? obj)(macro-readenv-wrap re (%syntax-for-name obj)))
       ((integer? obj)(macro-readenv-wrap re (bard:%syntax-integer obj)))
       ((flonum? obj)(macro-readenv-wrap re (bard:%syntax-float obj)))
       ((##ratnum? obj)(macro-readenv-wrap re (bard:%syntax-ratio obj)))
       (else (macro-readenv-wrap re (list 'literal obj)))))))

(define $constituent-chars
  (apply append
         (list
          (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (string->list "1234567890")
          (string->list "~!@$%^&*_-+=|:<>?/"))))

(define (bard:read-escaped-string re c)
  (let ((s (##read-escaped-string re c)))
    (bard:%syntax-text s)))

(define (bard:make-readtable)
  (let ((rt (##make-standard-readtable)))
    (for-each (lambda (ch)
                (##readtable-char-class-set! rt ch #f
                                             bard:read-number/keyword/symbol))
              $constituent-chars)
    (macro-readtable-keywords-allowed?-set! rt #f)
    (##readtable-char-class-set! rt #\' #t bard:read-quote)
    (##readtable-char-class-set! rt #\, #t bard:read-comma)
    (##readtable-char-class-set! rt #\\ #t bard:read-character)
    (##readtable-char-class-set! rt #\" #t bard:read-escaped-string)
    (macro-readtable-paren-keyword-set! rt 'ast:application)
    (macro-readtable-bracket-keyword-set! rt 'ast:sequence)
    (macro-readtable-brace-keyword-set! rt 'ast:record)
    rt))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------

(define $module-name-start-chars
  (string->list "abcdefghijklmnopqrstuvwxyz"))

(define $module-name-chars
  (string->list "abcdefghijklmnopqrstuvwxyz.1234567890"))

(define (%module-name-start? ch)
  (memv ch $module-name-start-chars))

(define (%module-name-char? ch)
  (memv ch $module-name-chars))

(define (%valid-module-name? mname)
  (and (string? mname)
       (> (string-length mname) 0)
       (%module-name-start? (string-ref mname 0))
       (string-every? %module-name-char? mname)
       (not (substring-position ".." mname string=?))
       (not (char=? #\. (string-ref mname (- (string-length mname) 1))))))

(define $variable-name-start-chars
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*_-+=|<>.?/"))

(define $variable-name-chars
  (string->list "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*_-+=|<>.?/"))

(define (%variable-name-start? ch)
  (memv ch $variable-name-start-chars))

(define (%variable-name-char? ch)
  (memv ch $variable-name-chars))

(define (%valid-variable-name? vname)
  (and (string? vname)
       (> (string-length vname) 0)
       (%variable-name-start? (string-ref vname 0))
       (string-every? %variable-name-char? vname)))

(define (%syntax-for-name namesym)
  (let* ((namestr (symbol->string namesym))
         (colon-pos (string-position #\: namestr char=?)))
    (if colon-pos
        (let* ((mname (substring namestr 0 colon-pos))
               (vname (substring namestr (+ colon-pos 1)(string-length namestr)))
               (bad-colon-pos (string-position #\: vname char=?)))
          (cond 
           (bad-colon-pos (error "Too many colons in a name" namesym))
           ((not (%valid-variable-name? vname))(error "Invalid variable name" vname))
           ((< (string-length mname) 1)(bard:%syntax-name 'bard.keyword (string->symbol vname)))
           ((not (%valid-module-name? mname))(error "Invalid module name" mname))
           (else (bard:%syntax-name (string->symbol mname) (string->symbol vname)))))
        (bard:%syntax-name #f (string->symbol namestr)))))

(define (bard:%read-syntax port)
  (let ((original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port (bard:make-readtable)))
        (lambda ()(let ((read-output (read port)))
                    (if (pair? read-output)
                        (let ((items (cdr read-output)))
                          (case (car read-output)
                            ((ast:sequence) (ast:sequence items))
                            ((ast:application) (ast:application (car items)(cdr items)))
                            ((ast:record) (ast:record (map (lambda (kv)
                                                             (ast:record-field (car kv)(cdr kv)))
                                                           (plist->alist items))))
                            (else (error "invalid syntax" read-output))))
                        read-output)))
        (lambda ()(input-port-readtable-set! port original-readtable)))))

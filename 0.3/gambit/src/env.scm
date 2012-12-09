;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************



(define (%null-env) '())

;;; a vspec can be any of:
;;; vname ; a symbol. value is #!unbound; setter is #f
;;; (vname) ; value is #!unbound; setter is #f
;;; (vname val) ; value is val; setter is #f
;;; (#f val) ; an anonymous variable (parameter) in a method call; value is val; setter is #f
;;; (vname val mutable: m) ; value is val; is m is true, setter is
;;;                          generated; otherwise it is #f

(define (%vspec->var vspec)
  (if (symbol? vspec)
      (%make-var vspec #!unbound mutable: #f)
      (if (pair? vspec)
          (let ((len (length vspec)))
            (cond
             ;; just the variable name
             ((and (= 1 len)
                   (symbol? (car vspec)))(%make-var (car vspec) #!unbound mutable: #f))
             ;; (vname val)
             ((and (= 2 len)
                   (symbol? (car vspec)))(%make-var (car vspec) (cadr vspec) mutable: #f))
             ;; the following case creates anonymous variables for environment frames
             ;; generated by method calls
             ((and (= 2 len)
                   (eqv? #f (car vspec)))(%make-var (car vspec) (cadr vspec) mutable: #f))
             ;; (vname val mutable: m)
             ((and (= 4 len)
                   (symbol? (car vspec))
                   (eq? mutable: (list-ref vspec 2)))
              (%make-var (car vspec) (cadr vspec) mutable: (and (list-ref vspec 2) #t)))
             (else (error (str "Invalid variable description: " vspec)))))
          (error (str "Invalid variable description: " vspec)))))

(define (%empty-frame)(vector))

(define (%make-frame vspecs)
  (list->vector (map %vspec->var vspecs)))

(define (%add-frame env frame)
  (cons frame env))

(define (%drop-frame env)
  (cdr env))

(define (%env-ref env i j)
  (vector-ref (list-ref env i) j))

(define (%find-variable env expr) 
  (let loop ((i 0)
             (frames env))
    (if (null? frames)
        (values #f #f)
        (let* ((frame (car frames))
               (varpos (vector-position expr frame test: (lambda (nm var)(eq? nm (%var-name var))))))
          (if varpos
              (values i varpos)
              (loop (+ 1 i)(cdr frames)))))))

(define (%lsetter env i j)
  (%var-setter (%env-ref env i j)))

(define (%lset! env i j v)
  (let* ((setter (%lsetter env i j)))
    (if setter
        (begin
          (setter v)
          v)
        (error (str "Cannot assign to an immutable variable: " (%var-name (%env-ref env i j)))))))


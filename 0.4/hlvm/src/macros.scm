;;;; ***********************************************************************
;;;;
;;;; Name:          macros.scm
;;;; Project:       Bard
;;;; Purpose:       built-in macro definitions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; macros built into the bard compiler
;;; these forms must be evaluated after the macro-definition forms are
;;; defined

(bard:compile-macro-definition
 '(macro not
    (^ (expr)
       (let ((args (cdr expr)))
         (if (nothing? args)
             (error "too few arguments to not")
             (if (nothing? (cdr args))
                 `(if ,(car args) #f #t)
                 (error "too many arguments to not")))))))

(bard:compile-macro-definition
 '(macro and
    (^ (expr)
       (let ((args (cdr expr)))
         (if (nothing? args)
             #t
             (let ((first-arg (car args))
                   (rest-args (cdr args)))
               (if (nothing? rest-args)
                   first-arg
                   `(if ,first-arg
                        (and ,@rest-args)
                        #f))))))))

(bard:compile-macro-definition
 '(macro or
    (^ (expr)
       (let ((args (cdr expr)))
         (if (nothing? args)
             #f
             (let ((first-arg (car args))
                   (rest-args (cdr args)))
               (if (nothing? rest-args)
                   first-arg
                   `(if ,first-arg
                        ,first-arg
                        (or ,@rest-args)))))))))

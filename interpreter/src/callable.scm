;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          callable.scm
;;;; Project:       Bard
;;;; Purpose:       representation of callable objects (functions, methods, continuations)
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; callable objects
;;; ---------------------------------------------------------------------

(define-type %callable
  id: E8EC72A9-7E1C-499D-A468-69D73C857B6C
  extender: defcallable
  (name %debug-name %set-debug-name!)
  (function %callable-function %set-callable-function!))

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(defcallable %method
  id: 927A1AD2-762A-4DE6-9900-C22857D20E5A
  extender: %def-method-type
  read-only:
  (formals %method-formals)
  (restarg %method-restarg)
  (required-count %method-required-count))

(%def-method-type %primitive-method constructor: %private-make-primitive-method)

(define (%make-primitive-method function  
                                #!key 
                                (parameters %nil)
                                (name #f)
                                (required-count 0)
                                (restarg #f))
  (%private-make-primitive-method name function parameters restarg required-count))

(define <primitive-method> (%define-structure '<primitive-method> (##structure-type (%make-primitive-method #f))))

(%def-method-type %interpreted-method
                  constructor: %private-make-interpreted-method
                  read-only:
                  (environment %method-environment %set-method-environment!)
                  (body %method-body))

(define (%method-lexical-environment env params rest vals)
  (let loop ((env env)
             (formals params)
             (args vals))
    (if (%null? args)
        ;; out of args
        (if (%null? formals)
            (if (%true? rest)
                (%add-binding env rest args)
                env)
            (error (string-append "Not enough arguments: " (%as-string vals))))
        ;; more args to process
        (if (%null? formals)
            (if (%true? rest)
                (%add-binding env rest args)
                (error (string-append "Too many arguments: " (%as-string vals))))
            (loop (%add-binding env (%car formals)(%car args))
                  (%cdr formals)
                  (%cdr args))))))

(define (%make-interpreted-method parameters method-body  
                                  #!key 
                                  (environment '())
                                  (name #f)
                                  (required-count 0)
                                  (restarg #f))
  (let* ((method (%private-make-interpreted-method name #f parameters restarg required-count environment method-body))
         (method-function (lambda args
                            (let* ((argcount (%length args)))
                              (if (< argcount required-count)
                                  (error (str "Expected " required-count "arguments, but found " (length args)))
                                  (let* ((env (%method-lexical-environment (%method-environment method) parameters restarg args)))
                                    (%eval method-body env)))))))
    (%set-callable-function! method method-function)
    method))

(define <interpreted-method>
  (%define-structure '<interpreted-method> (##structure-type (%make-interpreted-method '() '()))))

(define (%method-name? x)(or (symbol? x)(string? x)))


(defcallable %function
  id: 2538E21C-EEE0-44F7-89E6-7FFB5D445189
  constructor: %private-make-function
  (methods %function-methods)
  (thunk-method %function-thunk-method %set-function-thunk-method!))

(define (%make-function #!key (name 'anonymous-function))
  (let* ((fn (%private-make-function name #f (%singleton-tree) #f))
         (fn-function (lambda args
                        (let ((best-method (%function-best-method fn args)))
                          (if best-method
                              (%apply best-method args)
                              (error (str "No applicable method for " fn " with arguments " args)))))))
    (%set-callable-function! fn fn-function)
    fn))

(define <function> (%define-structure '<function> (##structure-type (%make-function))))

(define (%add-method! fn signature method)
  (let ((method-tree (%function-methods fn)))
    (if (null? signature)
        (%set-function-thunk-method! fn method)
        (apply %singleton-tree-put! method method-tree signature))
    fn))

(define (%add-primitive-method! fn msig params method-function #!key (name #f))
  (let* ((method (%make-primitive-method method-function 
                                         name: name parameters: params 
                                         required-count: (%length params))))
    (%add-method! fn msig method)
    fn))

(define (%search-method-tree-for-value mtree val)
  (let* ((sing (%existing-singleton val))
         (found (if sing (%singleton-tree-ref mtree sing) #f)))
    (or found
        (let* ((tp (%object->bard-type val))
               (found (%singleton-tree-ref mtree tp)))
          (or found
              (%singleton-tree-ref mtree Anything))))))

(define (%search-method-tree-for-values mtree vals)
  (if (null? vals)
      #f
      (let ((found (%search-method-tree-for-value mtree (car vals))))
        (if found
            (if (null? (cdr vals))
                (if (%singleton-tree? found)
                    #f
                    found)
                (if (%singleton-tree? found)
                    (%search-method-tree-for-values found (cdr vals))
                    #f))
            #f))))

(define (%function-best-method fn vals)
  (if (null? vals)
      (%function-thunk-method fn)
      (%search-method-tree-for-values (%function-methods fn) vals)))


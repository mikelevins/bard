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
;;; singleton-trees
;;; ---------------------------------------------------------------------
;;; used for storing function methods

(define-type %singleton-tree
  id: 3623F664-68ED-4642-BB14-B6FDCA33618C
  (entries %singleton-tree-entries))

(define (%singleton-tree)
  (make-%singleton-tree (make-table test: eq?)))

(define (%singleton-tree-ref tree key)
  (if (%singleton-tree? tree)
      (table-ref (%singleton-tree-entries tree) key #f)
      #f))

(define (%singleton-tree-get tree . keys)
  (if (%singleton-tree? tree)
      (if (null? keys)
          #f
          (let* ((entries (%singleton-tree-entries tree))
                 (subtree (table-ref entries (car keys) #f)))
            (if subtree
                (if (null? (cdr keys))
                    subtree
                    (apply %singleton-tree-get subtree (cdr keys)))
                #f)))
      #f))

(define (%singleton-tree-put! val tree . keys)
  (if (%singleton-tree? tree)
      (if (null? keys)
          (error (str "Invalid search path for %singleton-tree: " keys))
          (let* ((first-key (car keys))
                 (rest-keys (cdr keys))
                 (entries-table (%singleton-tree-entries tree)))
            (if (null? rest-keys)
                (table-set! entries-table first-key val)
                (let ((subtree (table-ref entries-table first-key #f)))
                  (if (not (%singleton-tree? subtree))
                      (begin
                        (set! subtree (%singleton-tree))
                        (table-set! entries-table first-key subtree)))
                  (apply %singleton-tree-put! val subtree (cdr keys))))))
      (error (str "Not a %singleton-tree: " tree))))

;;; ---------------------------------------------------------------------
;;; callable objects
;;; ---------------------------------------------------------------------

(define-type %callable
  id: E8EC72A9-7E1C-499D-A468-69D73C857B6C
  extender: defcallable
  (name %debug-name %set-debug-name!))

;;(defcallable %continuation)
;;(defcallable %prim)

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

(%def-method-type %primitive-method
                  constructor: %private-make-primitive-method
                  read-only:
                  (function %method-function))

(define (%make-primitive-method function  
                                #!key 
                                (parameters %nil)
                                (name #f)
                                (required-count 0)
                                (restarg #f))
  (%private-make-primitive-method name parameters restarg required-count function))

(define <primitive-method>
  (%define-standard-type '<primitive-method> (##structure-type (%make-primitive-method #f))))

(%def-method-type %interpreted-method
                  constructor: %private-make-interpreted-method
                  read-only:
                  (environment %method-environment %set-method-environment!)
                  (body %method-body))

(define (%make-interpreted-method parameters method-body  
                                  #!key 
                                  (environment '())
                                  (name #f)
                                  (required-count 0)
                                  (restarg #f))
  (%private-make-interpreted-method name parameters restarg required-count environment method-body))

(define <interpreted-method>
  (%define-standard-type '<interpreted-method> (##structure-type (%make-interpreted-method '() '()))))

(define (%method-name? x)(or (symbol? x)(string? x)))


(defcallable %function
  id: 2538E21C-EEE0-44F7-89E6-7FFB5D445189
  constructor: %private-make-function
  (methods %function-methods)
  (thunk-method %function-thunk-method %set-function-thunk-method!))

(define (%make-function #!key (name 'anonymous-function))
  (let ((fn (%private-make-function name (%singleton-tree) #f)))
    (%set-debug-name! fn name)
    fn))

(define <function> (%define-standard-type '<function> (##structure-type (%make-function))))

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

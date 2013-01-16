;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          singleton-tree.scm
;;;; Project:       Bard
;;;; Purpose:       a serach tree used for storing function methods
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

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

(define (%singleton-tree-remove! tree . keys)
  (if (%singleton-tree? tree)
      (if (null? keys)
          tree
          (let* ((entries (%singleton-tree-entries tree))
                 (subtree (table-ref entries (car keys) #f)))
            (if subtree
                (if (null? (cdr keys))
                    (table-set! entries (car keys))
                    (apply %singleton-tree-remove! subtree (cdr keys)))
                tree)))
      tree))

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

(define (%singleton-tree-enumerate stree #!optional (indent 0))
  (let ((entries (%singleton-tree-entries stree))
        (pad (make-string indent #\space)))
    (if (and entries (table? entries))
        (begin
          (table-for-each (lambda (k v)
                            (newline)
                            (display (str pad (%as-string k) ": "))
                            (if (%singleton-tree? v)
                                (%singleton-tree-enumerate v (+ indent 2))
                                (display (%as-string v))))
                          entries)))))


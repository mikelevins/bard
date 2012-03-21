;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          c3.scm
;;;; Project:       Categories
;;;; Purpose:       The C3 type linearization algorithm
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; About C3
;;; ----------------------------------------------------------------------
;;; C3 is the class-linearization algorithm described in:
;;; http://192.220.96.201/dylan/linearization-oopsla96.html It's a
;;; deterministic method of sorting a graph of class/superclass
;;; relations that gives fewer counterintuitive results than other
;;; common methods, and has proven in practice to be useful.  
;;;
;;; The C3 method of linearization works like this: start with a type
;;; T0. Fetch its direct supertypes, A0, B0, etc. T0 must maintain its
;;; supertypes in a stable order, and we merge that list in the same
;;; order, with T0 at the left end. Next, get the type linearizations
;;; of all the direct superclasses, arrange them in order
;;; left-to-right, and remove from them any types that are already in
;;; the output. 
;;;
;;; Next, choose a candidate from the left of one of the input
;;; lists. We want the leftmost candidate that does not appear in the
;;; tails of any of the inputs. If there is no such candidate, then
;;; the proposed class graph is inconsistent. If there is such a
;;; candidate, we append it to the right of the output and remove it
;;; from all the inputs. We repeat this process until all inputs are
;;; empty.
;;;
;;; This implementation of C3 makes no assumptions about the
;;; representations or relationships of types except that the
;;; direct-supertypes-fn and the all-supertypes-fn supplied as
;;; parameters to c3:compute-precedence each accept a single argument
;;; (a type, whatever that may mean), and each returns a correct list
;;; of types in stable order. The domain that uses C3 may represent
;;; types in any way it chooses, and may supply any functions it likes
;;; as long as these invariants are preserved. A typical domain might
;;; arrange for the parameters to c3:compute-precedence to cache the
;;; lists that they compute, so as to avoid unnecessary recomputation
;;; of the lists.

(define (c3:good-candidate? reject-lists cand)
  (and (not (any? (lambda (rl) (member cand rl)) 
                  reject-lists))
       cand))

(define (c3:find-candidate input-lists)
  (let ((ins (filter (lambda (i) (not (null? i))) input-lists)))
    (any? (partial c3:good-candidate? (map cdr ins)) (map car ins))))

(define (c3:remove-candidate cand input-lists)
  (map (lambda (ilist) (remove cand ilist equal?)) input-lists))

(define (c3:reduce-inputs input-lists output-list)
  (if (every? null? input-lists)
      (reverse output-list)
      (let ((cand (c3:find-candidate input-lists)))
        (if cand
            (c3:reduce-inputs (c3:remove-candidate cand input-lists)(cons cand output-list))
            (error "Inconsistent type graph")))))

;;; This implementation of C3 works with any input type, so long as we
;;; supply a function that can fetch all its direct supertypes in
;;; stable order (direct-supertypes-fn), and a function that can fetch
;;; the type-linearizations of its supertypes
;;; (all-supertypes-fn). These parameters are functions so that
;;; implementors of domains that use C3 can supply their own functions
;;; that work with their domains. Each is a function of a single
;;; parameter, the type. If a domain uses auxiliary data, then these
;;; functions should carry the auxiliary parameters in a closure.

(define (c3:compute-precedence t direct-supertypes-fn all-supertypes-fn)
  (let* ((direct-supers (direct-supertypes-fn t))
         (super-precedence-lists (map all-supertypes-fn direct-supers))
         (reversed-out (list t))
         (ins (append super-precedence-lists (list direct-supers))))
    (c3:reduce-inputs ins reversed-out)))


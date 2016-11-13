;;;; ***********************************************************************
;;;;
;;;; Name:          actor.scm
;;;; Project:       Bard
;;;; Purpose:       a single-thread of control for the bard vm
;;;; Author:        mikel evins
;;;; Copyright:     2012-2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define-structure actor
  id
  vm
  procedure
  halted?
  env
  stack
  code
  pc)

(define (new-actor #!key
                   (id (make-uuid))
                   (vm (default-vm))
                   (procedure #f)
                   (halted? #t)
                   (env '())
                   (stack '())
                   (code #f)
                   (pc 0))
  (make-actor id (or vm (default-vm)) procedure halted? env stack code pc))



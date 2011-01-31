;;;; -*- Mode: Lisp -*-

;;;; unification-package.lisp --
;;;; Package definition for the CL-UNIFICATION library.
;;;;
;;;; Copyright (c) 2004-2009 Marco Antoniotti
;;;; See file COPYING for licensing information.


(defpackage "IT.UNIMIB.DISCO.MA.CL.EXT.DACF.UNIFICATION" (:use "CL")
  (:nicknames  "CL.EXT.DACF.UNIFICATION" "UNIFY" "unify")
  (:documentation "The CL.EXT.DACF.UNIFICATION Package.

This package contains all the definitions necessary for the general
Common Lisp unifier to work.
The package also has the \"UNIFY\" nickname.")

  (:export
   "MAKE-TEMPLATE"
   "TEMPLATEP"
   "TEMPLATE-SPEC")

  (:export
   "*UNIFY-STRING-CASE-INSENSITIVE-P*"
   "UNIFY"
   "FIND-VARIABLE-VALUE"
   "V?"

   "MAKE-EMPTY-ENVIRONMENT"
   "APPLY-SUBSTITUTION"

   "UNIFICATION-FAILURE"
   "UNIFICATION-VARIABLE-UNBOUND"
   )

  (:export
   "ENVIRONMENT"
   "ENVIRONMENT-P")

  (:export
   "MATCH"
   "MATCHF"
   "MATCHING"
   "MATCH-CASE"
   "MATCHF-CASE"
   )

  (:export
   "UNIFY*"
   "UNIFY-EQUATIONS"
   "UNIFY-EQUATIONS*")

  #+cl-ppcre
  (:export
   "REGULAR-EXPRESSION"
   "REGEXP")
  )

;;;; end of file -- unification-package.lisp --

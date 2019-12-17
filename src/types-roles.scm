;;;; ***********************************************************************
;;;;
;;;; Name:          types-roles.scm
;;;; Project:       Bard
;;;; Purpose:       base Bard roles
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base structs
;;; =====================================================================

;;; ----------------------------------------------------------------------
;;; roles
;;; ----------------------------------------------------------------------

(define tags:$bard-role (%next-bard-type-number))
(define <role> (make-base-struct '<role> tags:$bard-role))


;;; constructor

(define (%make-role name)(make-role-instance <role> name))

;;; definitions of roles
;;; ----------------------------------------------------------------------
;;; convention: role names are nouns

(define & (%make-role '&)) ; the role of optional arguments
(define Anything (%make-role 'Anything))
(define Applicable (%make-role 'Applicable))
(define Boolean (%make-role 'Boolean))
(define Character (%make-role 'Character))
(define Role (%make-role 'Role))
(define File (%make-role 'File))
(define Float (%make-role 'Float))
(define Fraction (%make-role 'Fraction))
(define Function (%make-role 'Function))
(define InputStream (%make-role 'InputStream))
(define Integer (%make-role 'Integer))
(define IODirection (%make-role 'IODirection))
(define IOMode (%make-role 'IOMode))
(define IOType (%make-role 'IOType))
(define Keyword (%make-role 'Keyword))
(define List (%make-role 'List))
(define Method (%make-role 'Method))
(define Name (%make-role 'Name))
(define Null (%make-role 'Null))
(define Number (%make-role 'Number))
(define Orderable (%make-role 'Orderable))
(define OutputStream (%make-role 'OutputStream))
(define Pair (%make-role 'Pair))
(define Protocol (%make-role 'Protocol))
(define Ratio (%make-role 'Ratio))
(define Struct (%make-role 'Struct))
(define Stream (%make-role 'Stream))
(define Symbol (%make-role 'Symbol))
(define Table (%make-role 'Table))
(define Text (%make-role 'Text))
(define Type (%make-role 'Type))
(define Undefined (%make-role 'Undefined))
(define URL (%make-role 'URL))


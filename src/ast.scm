;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ast.scm
;;;; Project:       bard
;;;; Purpose:       abstract syntax trees for the reader
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; singletons
;;;---------------------------------------------------------------------

(define-type ast:undefined
  id: 723DC6F1-6DC5-4DA6-AC2E-4CBD31836F21
  constructor: ast:undefined)

(define-type ast:nothing
  id: C4A3BA8A-2753-472F-9563-2DB3CCF6E30E
  constructor: ast:nothing)

;;;---------------------------------------------------------------------
;;; booleans
;;;---------------------------------------------------------------------

(define-type ast:boolean
  id: 85AFA7AC-FD1B-4B3E-9E45-5D09B2A738AC
  constructor: ast:boolean
  (value ast:boolean-value))

;;;---------------------------------------------------------------------
;;; characters
;;;---------------------------------------------------------------------

(define-type ast:character
  id: F67D383B-41B4-4733-A45F-40BA054DEC99
  constructor: ast:character
  (value ast:character-value))

;;;---------------------------------------------------------------------
;;; numbers
;;;---------------------------------------------------------------------

(define-type ast:number
  id: EE71F810-4CE8-4FB4-99C7-3C2809180D4F
  constructor: ast:number
  (value ast:number-value))

;;;---------------------------------------------------------------------
;;; names
;;;---------------------------------------------------------------------

(define-type ast:name
  id: A80B621F-9753-4B6A-AF44-E252C128EC0C
  constructor: ast:name
  (symbol ast:name-symbol)
  (module ast:name-module))

;;;---------------------------------------------------------------------
;;; text
;;;---------------------------------------------------------------------

(define-type ast:text
  id: 30CED4E8-5EB0-4A79-9DA6-9916EF3465AB
  constructor: ast:text
  (value ast:text-value))

;;;---------------------------------------------------------------------
;;; sequences and records
;;;---------------------------------------------------------------------

(define-type ast:sequence
  id: ACC2DEE2-B2FA-41CA-81A6-CDADDBA5B0E7
  constructor: ast:sequence
  (elements ast:sequence-elements))

(define-type ast:application
  id: B866812E-7833-4284-886B-6C8D256601E7
  constructor: ast:application
  (operator ast:application-operator)
  (parameters ast:application-parameters))

(define-type ast:record-field
  id: A64E2C7B-D48F-4D4A-8C84-939EA14E1024
  constructor: ast:record-field
  (key ast:record-field-key)
  (val ast:record-field-val))

(define-type ast:record
  id: 3ACC5C6E-AF43-4A12-B5AD-4B2937F5EF79
  constructor: ast:record
  (fields ast:record-fields))



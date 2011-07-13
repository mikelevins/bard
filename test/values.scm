;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       tests for built-in Bard types
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; <eof>->EOF
;;; ---------------------------------------------------------------------

(define $eof (<eof>:make))
(<eof>:eof? $eof)

;;; ---------------------------------------------------------------------
;;; <undefined>->Undefined
;;; ---------------------------------------------------------------------

(<undefined>:undefined? (<undefined>:undefined))
(<undefined>:defined? (<undefined>:undefined))

;;; ---------------------------------------------------------------------
;;; <nothing>->Nothing
;;; ---------------------------------------------------------------------

(define $nada (<nothing>:make))
(<nothing>:nothing? $nada)
(<nothing>:something? $nada)

;;; ---------------------------------------------------------------------
;;; <boolean>->Boolean
;;; ---------------------------------------------------------------------

(<boolean>:false? (<boolean>:false))
(<boolean>:false? (<boolean>:true))
(<boolean>:true? (<boolean>:false))
(<boolean>:true? (<boolean>:true))
(<boolean>:boolean? (<boolean>:false))
(<boolean>:boolean? (<boolean>:true))
(<boolean>:boolean? 0)

;;; ---------------------------------------------------------------------
;;; <ralist>->Sequence
;;; ---------------------------------------------------------------------

(define $l0 (<ralist>:empty))
(<ralist>:empty? $l0)
(<ralist>:ralist? $l0)

(define $l1 (<ralist>:add-first 1 $l0))
(<ralist>:empty? $l1)
(<ralist>:ralist? $l1)
(<ralist>:first $l1)
(<ralist>:rest $l1)

;;; ---------------------------------------------------------------------
;;; <cell>->Cell
;;; ---------------------------------------------------------------------

(define $c (<cell>:make))
(<cell>:get $c)
(<cell>:put! $c 0)
(<cell>:put! $c 1)

;;; ---------------------------------------------------------------------
;;; <slot>->Slot
;;; ---------------------------------------------------------------------

(define $s (<slot>:make name: "Fred"))
(<slot>:key $s)
(<slot>:value $s)

;;; ---------------------------------------------------------------------
;;; <frame>->Map
;;; ---------------------------------------------------------------------

(define $f (<frame>:plist->frame (list name: "Fred" age: 45 size: 'large)))
(<frame>:empty? $f)
(<frame>:frame? $f)
(<frame>:get $f name:)
(<frame>:get $f size:)
(<frame>:get $f hair:)

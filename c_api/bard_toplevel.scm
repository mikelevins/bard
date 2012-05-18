;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard_toplevel.scm
;;;; Project:       Bard
;;;; Purpose:       Exposing Scheme API functions to the Bard toplevel
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; foreign types
;;; ---------------------------------------------------------------------

(%defglobal 'objc:class-name objc:class-name)
(%defglobal 'objc:object-class-name objc:object-class-name)
(%defglobal 'objc:get-class objc:get-class)
(%defglobal 'objc:class-of objc:class-of)
(%defglobal 'objc:instance-of? objc:instance-of?)
(%defglobal 'objc:direct-instance-of? objc:direct-instance-of?)

;;; ---------------------------------------------------------------------
;;; object lifecycle
;;; ---------------------------------------------------------------------

(%defglobal 'objc:retain objc:retain)
(%defglobal 'objc:release objc:release)
(%defglobal 'objc:autorelease objc:autorelease)

;;; ---------------------------------------------------------------------
;;; NSMutableArray
;;; ---------------------------------------------------------------------

(%defglobal 'objc:make-NSMutableArray objc:make-NSMutableArray)
(%defglobal 'objc:NSMutableArray/count objc:NSMutableArray/count)
(%defglobal 'objc:NSMutableArray/add-string! objc:NSMutableArray/add-string!)
(%defglobal 'objc:NSMutableArray/string-at-index objc:NSMutableArray/string-at-index)
(%defglobal 'objc:string-list->NSMutableArray objc:string-list->NSMutableArray)
(%defglobal 'objc:NSMutableArray->List objc:NSMutableArray->List)

;;; ---------------------------------------------------------------------
;;; NSMutableDictionary
;;; ---------------------------------------------------------------------

(%defglobal 'objc:make-NSMutableDictionary objc:make-NSMutableDictionary)
(%defglobal 'objc:NSMutableDictionary/put-string-at-string! objc:NSMutableDictionary/put-string-at-string!)
(%defglobal 'objc:NSMutableDictionary/get-string-at-string objc:NSMutableDictionary/get-string-at-string)
(%defglobal 'objc:NSMutableDictionary/put-int-at-string! objc:NSMutableDictionary/put-int-at-string!)
(%defglobal 'objc:NSMutableDictionary/get-int-at-string objc:NSMutableDictionary/get-int-at-string)
(%defglobal 'objc:NSMutableDictionary/put-float-at-string! objc:NSMutableDictionary/put-float-at-string!)
(%defglobal 'objc:NSMutableDictionary/get-float-at-string objc:NSMutableDictionary/get-float-at-string)
(%defglobal 'objc:NSMutableDictionary/put-bool-at-string! objc:NSMutableDictionary/put-bool-at-string!)
(%defglobal 'objc:NSMutableDictionary/get-bool-at-string objc:NSMutableDictionary/get-bool-at-string)

;;; ---------------------------------------------------------------------
;;; conversions
;;; ---------------------------------------------------------------------

(%defglobal 'objc:string->NSString objc:string->NSString)
(%defglobal 'objc:NSString->string objc:NSString->string)


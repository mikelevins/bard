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
;;; Constructors
;;; ---------------------------------------------------------------------

(%defglobal 'objc:make-NSMutableArray objc:make-NSMutableArray)
(%defglobal 'objc:make-NSMutableDictionary objc:make-NSMutableDictionary)

;;; ---------------------------------------------------------------------
;;; Accessors
;;; ---------------------------------------------------------------------

(%defglobal 'objc:NSMutableArray/add-object! objc:NSMutableArray/add-object!)
(%defglobal 'objc:NSArray/count objc:NSArray/count)
(%defglobal 'objc:NSArray/object-at-index objc:NSArray/object-at-index)
(%defglobal 'objc:NSMutableDictionary/set-object-for-key! objc:NSMutableDictionary/set-object-for-key!)
(%defglobal 'objc:NSDictionary/object-for-key objc:NSDictionary/object-for-key)
(%defglobal 'objc:to-NSDictionary-key objc:to-NSDictionary-key)
(%defglobal 'objc:NSDictionary/get-keys-array objc:NSDictionary/get-keys-array)

;;; ---------------------------------------------------------------------
;;; Conversions
;;; ---------------------------------------------------------------------

(%defglobal 'objc:NSNumber? objc:NSNumber?)
(%defglobal 'objc:NSString? objc:NSString?)
(%defglobal 'objc:NSArray? objc:NSArray?)
(%defglobal 'objc:NSDictionary? objc:NSDictionary?)
(%defglobal 'objc:boolean->NSNumber objc:boolean->NSNumber)
(%defglobal 'objc:integer->NSNumber objc:integer->NSNumber)
(%defglobal 'objc:float->NSNumber objc:float->NSNumber)
(%defglobal 'objc:string->NSString objc:string->NSString)
(%defglobal 'objc:list->NSArray objc:list->NSArray)
(%defglobal 'objc:frame->NSDictionary objc:frame->NSDictionary)
(%defglobal 'objc:value-from-NSNumber objc:value-from-NSNumber)
(%defglobal 'objc:string-from-NSString objc:string-from-NSString)
(%defglobal 'objc:list-from-NSArray objc:list-from-NSArray)
(%defglobal 'objc:frame-from-NSDictionary objc:frame-from-NSDictionary)
(%defglobal 'objc:to-objc objc:to-objc)
(%defglobal 'objc:from-objc objc:from-objc)




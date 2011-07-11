;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prototypes.scm
;;;; Project:       bard
;;;; Purpose:       frames representing the basic built-in types
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $type-debug-names (make-table))

(define (def-debug-name type nm)
  (table-set! $type-debug-names type nm))

(define (debug-name x)
  (or (table-ref $type-debug-names x #f)
      (object->string x)))

(define <undefined> 
  (frame:plist->frame 
   `(built-in-type: bard:undefined)))
(def-debug-name <undefined> "<undefined>")

(define <nothing>
  (frame:plist->frame 
   `(built-in-type: bard:nothing)))
(def-debug-name <nothing> "<nothing>")

(define <boolean>
  (frame:plist->frame 
   `(built-in-type: bard:boolean)))
(def-debug-name <boolean> "<boolean>")

(define <integer>
  (frame:plist->frame 
   `(built-in-type: bard:integer)))
(def-debug-name <integer> "<integer>")

(define <flonum>
  (frame:plist->frame 
   `(built-in-type: bard:flonum)))
(def-debug-name <flonum> "<flonum>")

(define <ratnum>
  (frame:plist->frame 
   `(built-in-type: bard:ratnum)))
(def-debug-name <ratnum> "<ratnum>")

(define <name>
  (frame:plist->frame 
   `(built-in-type: bard:name)))
(def-debug-name <name> "<name>")

(define <text>
  (frame:plist->frame 
   `(built-in-type: bard:text)))
(def-debug-name <text> "<text>")

(define <sequence>
  (frame:plist->frame 
   `(built-in-type: bard:sequence)))
(def-debug-name <sequence> "<sequence>")

(define <cell>
  (frame:plist->frame 
   `(built-in-type: bard:cell)))
(def-debug-name <cell> "<cell>")

(define <slot>
  (frame:plist->frame 
   `(built-in-type: bard:slot)))
(def-debug-name <slot> "<slot>")

(define <frame>
  (frame:plist->frame 
   `(built-in-type: bard:frame)))
(def-debug-name <frame> "<frame>")

(define <port>
  (frame:plist->frame 
   `(built-in-type: bard:port)))
(def-debug-name <port> "<port>")


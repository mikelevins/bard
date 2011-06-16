;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prototypes.scm
;;;; Project:       bard
;;;; Purpose:       frames representing the basic built-in types
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define <undefined> 
  (frame:plist->frame 
   `(built-in-type: bard:undefined)))

(define <nothing>
  (frame:plist->frame 
   `(built-in-type: bard:nothing)))

(define <boolean>
  (frame:plist->frame 
   `(built-in-type: bard:boolean)))

(define <number>
  (frame:plist->frame 
   `(built-in-type: bard:number)))

(define <text>
  (frame:plist->frame 
   `(built-in-type: bard:text)))

(define <sequence>
  (frame:plist->frame 
   `(built-in-type: bard:sequence)))

(define <cell>
  (frame:plist->frame 
   `(built-in-type: bard:cell)))

(define <slot>
  (frame:plist->frame 
   `(built-in-type: bard:slot)))

(define <frame>
  (frame:plist->frame 
   `(built-in-type: bard:frame)))

(define <port>
  (frame:plist->frame 
   `(built-in-type: bard:port)))


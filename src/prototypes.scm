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

(define <frame>
  (frame:plist->frame 
   `(built-in-type: bard:frame)))

(define <process>
  (frame:plist->frame 
   `(built-in-type: bard:process)))


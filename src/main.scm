;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.scm
;;;; Project:       Bard
;;;; Purpose:       toplevel entry point for the Bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(c-declare "#import <Foundation/Foundation.h>")

(define objc:init-autorelease-pool
  (c-lambda () (pointer "void")
#<<c-code
   NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
   ___result_voidstar = pool;
c-code
))

(define objc:drain-autorelease-pool
  (c-lambda ((pointer "void")) void
#<<c-code
  NSAutoreleasePool * pool = (NSAutoreleasePool*)___arg1;
  [pool drain];
c-code
))


(let ((pool (objc:init-autorelease-pool)))
  (bard:repl)
  (objc:drain-autorelease-pool pool))

;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          objc_data.scm
;;;; Project:       Bard
;;;; Purpose:       Scheme functions for constructing Objective-C data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(c-declare "#import <Foundation/Foundation.h>")

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------


(define objc:string->NSString
  (c-lambda (char-string) (pointer "NSString")
#<<c-code
   NSString* s = [NSString stringWithCString: ___arg1 encoding:NSASCIIStringEncoding];
   ___result_voidstar = (void*)s;
c-code
))

(define objc:NSString->string
  (c-lambda ((pointer "NSString")) char-string
#<<c-code
   const char* s = [___arg1 cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = s;
c-code
))

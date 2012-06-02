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
   char* s = (char *)[___arg1 cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = s;
c-code
))

(define objc:make-NSMutableArray
  (c-lambda () (pointer "NSMutableArray")
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   ___result_voidstar = (void*)arr;
c-code
))

(define objc:NSMutableArray/add-string!
  (c-lambda ((pointer "NSMutableArray") (pointer "NSString")) void
#<<c-code
   [___arg1 addObject:___arg2];
c-code
))

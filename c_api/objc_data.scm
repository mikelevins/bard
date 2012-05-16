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

(define (objc:nil) #f)

(define objc:retain
  (c-lambda ("id") void
#<<c-code
   [___arg1 retain];
c-code
))

(define objc:release
  (c-lambda ("id") void
#<<c-code
   [___arg1 release];
c-code
))

(define objc:make-ns-string
  (c-lambda (char-string) "id"
#<<c-code
   NSString* s = [NSString stringWithCString:___arg1 encoding:NSASCIIStringEncoding];
   ___result_voidstar = (void*)s;
c-code
))

(define objc:make-ns-mutable-array
  (c-lambda () "id"
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   ___result_voidstar = (void*)arr;
c-code
))

(define objc:make-ns-mutable-dictionary
  (c-lambda () "id"
#<<c-code
   NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithCapacity:16];
   ___result_voidstar = (void*)dict;
c-code
))

(define objc:ns-mutable-dictionary/set-string-for-key
  (c-lambda ((pointer "NSMutableDictionary") char-string char-string) void
#<<c-code
   NSMutableDictionary* dict =___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   NSString* val = [NSString stringWithCString: ___arg3 encoding: NSASCIIStringEncoding];
   [dict setObject: val forKey:key];
c-code
))


(define objc:ns-mutable-array/add-string
  (c-lambda ((pointer "NSMutableArray")(pointer "NSString")) "id"
#<<c-code
   [___arg1 addObject: ___arg2];
   ___result_voidstar = (void*)___arg1;
c-code
))

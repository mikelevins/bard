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

(define objc:describe
  (c-lambda ("id") void
#<<c-code
c-code
))

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

(define objc:string-as-ns-string
  (c-lambda (char-string) (pointer "NSString")
#<<c-code
   NSString* s = [NSString stringWithCString:___arg1 encoding:NSASCIIStringEncoding];
   ___result_voidstar = (void*)s;
c-code
))

(define objc:ns-string-as-string
  (c-lambda ((pointer "NSString")) char-string
#<<c-code
   char* cstr = [___arg1 cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = cstr;
c-code
))

(define objc:make-ns-mutable-array
  (c-lambda () (pointer "NSMutableArray")
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   ___result_voidstar = (void*)arr;
c-code
))

(define objc:make-ns-mutable-dictionary
  (c-lambda () (pointer "NSMutableDictionary")
#<<c-code
   NSMutableDictionary* dict = [NSMutableDictionary dictionary];
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

(define objc:ns-mutable-dictionary/get-string-for-key
  (c-lambda ((pointer "NSMutableDictionary") char-string) char-string
#<<c-code
   NSMutableDictionary* dict =___arg1;
   NSString* key = ___arg2;
   NSString* str = (NSString*)[dict objectForKey: key];
   char* cstr = [str cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = cstr;
c-code
))


(define objc:ns-mutable-array/add-string
  (c-lambda ((pointer "NSMutableArray")(pointer "NSString")) void
#<<c-code
   [___arg1 addObject: ___arg2];
c-code
))

(define objc:ns-mutable-array/count
  (c-lambda ((pointer "NSMutableArray")) int
#<<c-code
   int c = [___arg1 count];
   ___result = c;
c-code
))

(define objc:ns-mutable-array/element-as-string
  (c-lambda ((pointer "NSMutableArray") int) char-string
#<<c-code
   NSString* str = (NSString*)[___arg1 objectAtIndex: ___arg2];
   char* cstr = [str cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = cstr;
c-code
))

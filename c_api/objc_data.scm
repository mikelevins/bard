;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          objc_data.scm
;;;; Project:       Bard
;;;; Purpose:       Scheme functions for constructing Objective-C data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(c-declare "#include <objc/runtime.h>")
(c-declare "#import <Foundation/Foundation.h>")

;;; ---------------------------------------------------------------------
;;; object lifecycle
;;; ---------------------------------------------------------------------

(define objc:class-name
  (c-lambda ("id") char-string
#<<c-code
   const char* cname = object_getClassName(___arg1);
   ___result = cname;
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

(define objc:autorelease
  (c-lambda ("id") void
#<<c-code
   [___arg1 autorelease];
c-code
))

;;; ---------------------------------------------------------------------
;;; NSString
;;; ---------------------------------------------------------------------

(define objc:string->NSString
  (c-lambda (char-string) (pointer "NSString")
#<<c-code
   NSString* s = [NSString stringWithCString:___arg1 encoding:NSASCIIStringEncoding];
   ___result_voidstar = (void*)s;
c-code
))

(define objc:NSString->string
  (c-lambda ((pointer "NSString")) char-string
#<<c-code
   char* cstr = [___arg1 cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = cstr;
c-code
))

;;; ---------------------------------------------------------------------
;;; NSMutableArray
;;; ---------------------------------------------------------------------

(define objc:make-NSMutableArray
  (c-lambda () (pointer "NSMutableArray")
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   ___result_voidstar = (void*)arr;
c-code
))

(define objc:NSMutableArray/count
  (c-lambda ((pointer "NSMutableArray")) int
#<<c-code
   int n = [___arg1 count];
   ___result = n;
c-code
))

(define objc:NSMutableArray/add-string!
  (c-lambda ((pointer "NSMutableArray") char-string) void
#<<c-code
   NSString* str = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   [___arg1 addObject: str];
c-code
))

(define objc:NSMutableArray/string-at-index
  (c-lambda ((pointer "NSMutableArray") int) char-string
#<<c-code
   NSMutableArray* theArray = ___arg1;
   int index = ___arg2;
   int count = [theArray count];
   if (index >= count) {
      ___result = NULL;
    } else {
      id elt = [theArray objectAtIndex:index];
      if ([elt isKindOfClass:[NSString class]]) {
        NSString* str = (NSString*)elt;
        char* cstr = [str cStringUsingEncoding:NSASCIIStringEncoding];
        ___result = cstr;
      } else {
        ___result = NULL;
      }
    }
c-code
))

(define (objc:string-list->NSMutableArray ls)
  (if (and (list? ls)
           (every? string? ls))
      (let ((arr (objc:make-NSMutableArray)))
        (for-each (lambda (s) (objc:NSMutableArray/add-string! arr s)) ls)
        arr)
      (error (string-append "invalid argument in objc:list->NSMutableArray: "
                            (object->string ls)))))


;;; ---------------------------------------------------------------------
;;; NSMutableDictionary
;;; ---------------------------------------------------------------------

(define objc:make-NSMutableDictionary
  (c-lambda () (pointer "NSMutableDictionary")
#<<c-code
   NSMutableDictionary* dict = [NSMutableDictionary dictionary];
   ___result_voidstar = (void*)dict;
c-code
))

(define objc:NSMutableDictionary/put-string-at-string!
  (c-lambda ((pointer "NSMutableDictionary") char-string char-string) void
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   NSString* val = [NSString stringWithCString: ___arg3 encoding: NSASCIIStringEncoding];
   [dict setValue: val forKey: key];
c-code
))

(define objc:NSMutableDictionary/get-string-at-string
  (c-lambda ((pointer "NSMutableDictionary") char-string) char-string
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   id val = [dict valueForKey: key];
   if (val == nil) {
        ___result = NULL;
   } else {
      if ([val isKindOfClass:[NSString class]]) {
        NSString* str = (NSString*)val;
        char* cstr = [str cStringUsingEncoding:NSASCIIStringEncoding];
        ___result = cstr;
      } else {
        ___result = NULL;
      }
    }
c-code
))

(define objc:NSMutableDictionary/get-string-at-string
  (c-lambda ((pointer "NSMutableDictionary") char-string) char-string
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   id val = [dict valueForKey: key];
   if (val == nil) {
        ___result = NULL;
   } else {
      if ([val isKindOfClass:[NSString class]]) {
        NSString* str = (NSString*)val;
        char* cstr = [str cStringUsingEncoding:NSASCIIStringEncoding];
        ___result = cstr;
      } else {
        ___result = NULL;
      }
    }
c-code
))

(define objc:NSMutableDictionary/put-int-at-string!
  (c-lambda ((pointer "NSMutableDictionary") char-string int) void
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   int val = ___arg3;
   NSNumber* num = [NSNumber numberWithInt: val];
   [dict setValue: num forKey: key];
c-code
))

(define objc:NSMutableDictionary/get-int-at-string
  (c-lambda ((pointer "NSMutableDictionary") char-string) int
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   id val = [dict valueForKey: key];
   if (val == nil) {
        ___result = INT_MIN;
   } else {
      if ([val isKindOfClass:[NSNumber class]]) {
        int i = [val intValue];
        ___result = i;
      } else {
        ___result = INT_MIN;
      }
    }
c-code
))

(define objc:NSMutableDictionary/put-float-at-string!
  (c-lambda ((pointer "NSMutableDictionary") char-string float) void
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   float val = ___arg3;
   NSNumber* num = [NSNumber numberWithFloat: val];
   [dict setValue: num forKey: key];
c-code
))

(define objc:NSMutableDictionary/get-float-at-string
  (c-lambda ((pointer "NSMutableDictionary") char-string) float
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   id val = [dict valueForKey: key];
   if (val == nil) {
        ___result = FLT_MIN;
   } else {
      if ([val isKindOfClass:[NSNumber class]]) {
        float f = [val floatValue];
        ___result = f;
      } else {
        ___result = FLT_MIN;
      }
    }
c-code
))

(define objc:NSMutableDictionary/put-bool-at-string!
  (c-lambda ((pointer "NSMutableDictionary") char-string bool) void
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   BOOL val = ___arg3;
   NSNumber* num = [NSNumber numberWithBool: val];
   [dict setValue: num forKey: key];
c-code
))

(define objc:NSMutableDictionary/get-bool-at-string
  (c-lambda ((pointer "NSMutableDictionary") char-string) bool
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   id val = [dict valueForKey: key];
   if (val == nil) {
        ___result = NO;
   } else {
      if ([val isKindOfClass:[NSNumber class]]) {
        BOOL b = [val boolValue];
        ___result = b;
      } else {
        ___result = NO;
      }
    }
c-code
))



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
;;; foreign types
;;; ---------------------------------------------------------------------

(define objc:get-class
  (c-lambda (char-string) "id"
#<<c-code
   id class = objc_getClass(___arg1);
   ___result_voidstar = (void*)class;
c-code
))

(define objc:class-of
  (c-lambda ("id") "Class"
#<<c-code
   Class class = [___arg1 class];
   ___result_voidstar = (void*)class;
c-code
))

(define objc:instance-of?
  (c-lambda ("id" "Class") bool
#<<c-code
   BOOL result = [___arg1 isKindOfClass: ___arg2];
   ___result = result;
c-code
))

(define objc:direct-instance-of?
  (c-lambda ("id" "Class") bool
#<<c-code
   BOOL result = [___arg1 isMemberOfClass: ___arg2];
   ___result = result;
c-code
))


;;; ---------------------------------------------------------------------
;;; object lifecycle
;;; ---------------------------------------------------------------------

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
;;; Constructors
;;; ---------------------------------------------------------------------

(define objc:make-NSMutableArray
  (c-lambda () (pointer "NSMutableArray")
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   ___result_voidstar = (void*)arr;
c-code
))

(define objc:make-NSMutableDictionary
  (c-lambda () (pointer "NSMutableDictionary")
#<<c-code
   NSMutableDictionary* dict = [NSMutableDictionary dictionary];
   ___result_voidstar = (void*)dict;
c-code
))

;;; ---------------------------------------------------------------------
;;; Accessors
;;; ---------------------------------------------------------------------

(define objc:NSMutableArray/add-object!
  (c-lambda ((pointer "NSMutableArray") "id") void
#<<c-code
   NSMutableArray* arr = ___arg1;
   id obj = ___arg2;
   [arr addObject: obj];
c-code
))

(define objc:NSArray/count
  (c-lambda ((pointer "NSArray" (NSArray* NSMutableArray*))) int
#<<c-code
   NSArray* arr = ___arg1;
   int n = [arr count];
   ___result = n;
c-code
))

(define objc:NSArray/object-at-index
  (c-lambda ((pointer "NSArray" (NSArray* NSMutableArray*)) int) "id"
#<<c-code
   NSArray* arr = ___arg1;
   int n = ___arg2;
   id obj = [arr objectAtIndex: n];
   ___result_voidstar = (void*)obj;
c-code
))

(define objc:NSMutableDictionary/set-object-for-key!
  (c-lambda ((pointer "NSMutableDictionary")(pointer "NSString") "id") void
#<<c-code
   NSMutableDictionary* dict = ___arg1;
   NSString* key = ___arg2;
   id obj = ___arg3;
   [dict setObject: obj forKey: key];
c-code
))

(define objc:NSDictionary/object-for-key
  (c-lambda ((pointer "NSDictionary" (NSDictionary* NSMutableDictionary*))(pointer "NSString"))  "id"
#<<c-code
   NSDictionary* dict = ___arg1;
   NSString* key = ___arg2;
   id obj = [dict objectForKey: key];
   ___result_voidstar = (void*)obj;
c-code
))

(define (objc:to-NSDictionary-key key)
  (let ((k (cond
            ((string? key) key)
            ((symbol? key) (symbol->string key))
            ((keyword? key) (keyword->string key))
            (else (error (string-append "Invalid NSDictionary key: " (object->string key)))))))
    (objc:string->NSString k)))

(define objc:NSDictionary/get-keys-array
  (c-lambda ((pointer "NSDictionary" (NSDictionary* NSMutableDictionary*))) (pointer "NSArray")
#<<c-code
   NSDictionary* dict = ___arg1;
   NSArray* keys = [dict allKeys];
   ___result_voidstar = (void*)keys;
c-code
))

;;; ---------------------------------------------------------------------
;;; Conversions
;;; ---------------------------------------------------------------------

(define (objc:NSNumber? value)
  (objc:instance-of? value (objc:get-class "NSNumber")))

(define (objc:NSString? value)
  (objc:instance-of? value (objc:get-class "NSString")))

(define (objc:NSArray? value)
  (objc:instance-of? value (objc:get-class "NSArray")))

(define (objc:NSDictionary? value)
  (objc:instance-of? value (objc:get-class "NSDictionary")))

(define objc:boolean->NSNumber
  (c-lambda (bool) "id"
#<<c-code
   NSNumber* num = [NSNumber numberWithBool: ___arg1];
   ___result_voidstar = (void*)num;
c-code
))

(define objc:integer->NSNumber
  (c-lambda (int) "id"
#<<c-code
   NSNumber* num = [NSNumber numberWithInt: ___arg1];
   ___result_voidstar = (void*)num;
c-code
))

(define objc:float->NSNumber
  (c-lambda (float) "id"
#<<c-code
   NSNumber* num = [NSNumber numberWithFloat: ___arg1];
   ___result_voidstar = (void*)num;
c-code
))

(define objc:string->NSString
  (c-lambda (char-string) (pointer "NSString")
#<<c-code
   NSString* s = [NSString stringWithCString: ___arg1 encoding:NSASCIIStringEncoding];
   ___result_voidstar = (void*)s;
c-code
))

(define (objc:list->NSArray ls)
  (let ((arr (objc:make-NSMutableArray)))
    (for-each (lambda (value)
                (let ((obj (objc:to-objc value)))
                  (objc:NSMutableArray/add-object! arr obj)))
              ls)
    arr))

(define (objc:frame->NSDictionary fr)
  (let ((dict (objc:make-NSMutableDictionary)))
    (let loop ((keys (%keys fr)))
      (if (null? keys)
          dict
          (let* ((key (car keys))
                 (objc-key (objc:to-NSDictionary-key key))
                 (val (%frame-get fr key))
                 (objc-val (objc:to-objc val)))
            (objc:NSMutableDictionary/set-object-for-key! dict objc-key objc-val)
            (loop (cdr keys)))))))

(define objc:value-from-NSNumber
  (c-lambda ((pointer "NSNumber")) float
#<<c-code
   NSNumber* num = ___arg1;
   float result = [num floatValue];
   ___result = result;
c-code
))

(define objc:string-from-NSString
  (c-lambda ((pointer "NSString")) char-string
#<<c-code
   char* cstr = [___arg1 cStringUsingEncoding:NSASCIIStringEncoding];
   ___result = cstr;
c-code
))

(define (objc:list-from-NSArray arr)
  (let ((array-count (objc:NSArray/count arr)))
    (let loop ((i (- array-count 1))
               (result '()))
      (if (< i 0)
          result
          (loop (- i 1)
                (cons (objc:from-objc (objc:NSArray/object-at-index arr i)) result))))))

(define (objc:frame-from-NSDictionary dict)
  (let* ((keys (objc:NSDictionary/get-keys-array dict))
         (keycount (objc:NSArray/count keys)))
    (let loop ((i (- keycount 1))
               (slots '()))
      (if (< i 0)
          (%make-frame slots)
          (let* ((key (objc:from-objc (objc:NSArray/object-at-index keys i)))
                 (val (objc:from-objc (objc:NSDictionary/object-for-key dict key))))
            (loop (- i 1)
                  (cons (list key val)
                        slots)))))))

(define (objc:to-objc value)
  (cond
   ((boolean? value)(objc:boolean->NSNumber value))
   ((integer? value)(objc:integer->NSNumber value))
   ((flonum? value)(objc:float->NSNumber value))
   ((string? value)(objc:string->NSString value))
   ((list? value)(objc:list->NSArray value))
   ((%frame? value)(objc:frame->NSDictionary value))
   (else (error (string-append "Don't know how to convert " (object->string value) " to Objective-C")))))


(define (objc:from-objc value)
  (cond
   ((objc:NSNumber? value)(objc:value-from-NSNumber value))
   ((objc:NSString? value)(objc:string-from-NSString value))
   ((objc:NSArray? value)(objc:list-from-NSArray value))
   ((objc:NSDictionary? value)(objc:frame-from-NSDictionary value))
   (else (error (string-append "Don't know how to convert " (object->string value) " from Objective-C")))))




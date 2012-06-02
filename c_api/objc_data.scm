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
;;; conversions
;;; ---------------------------------------------------------------------

;;; scalars

(define objc:boolean->NSNumber
  (c-lambda (bool) (pointer "NSNumber")
#<<c-code
   NSNumber* num = [NSNumber numberWithBool:___arg1];
   ___result_voidstar = (void*)num;
c-code
))

(define objc:integer->NSNumber
  (c-lambda (int) (pointer "NSNumber")
#<<c-code
   NSNumber* num = [NSNumber numberWithInt:___arg1];
   ___result_voidstar = (void*)num;
c-code
))

(define objc:float->NSNumber
  (c-lambda (float) (pointer "NSNumber")
#<<c-code
   NSNumber* num = [NSNumber numberWithFloat:___arg1];
   ___result_voidstar = (void*)num;
c-code
))

;;; strings

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

;;; lists

(define objc:make-NSMutableArray
  (c-lambda () (pointer "NSMutableArray")
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   ___result_voidstar = (void*)arr;
c-code
))

(define objc:NSMutableArray/add-number!
  (c-lambda ((pointer "NSMutableArray") (pointer "NSNumber")) void
#<<c-code
   [___arg1 addObject:___arg2];
c-code
))

(define objc:NSMutableArray/add-string!
  (c-lambda ((pointer "NSMutableArray") (pointer "NSString")) void
#<<c-code
   [___arg1 addObject:___arg2];
c-code
))

(define objc:NSMutableArray/add-array!
  (c-lambda ((pointer "NSMutableArray") (pointer "NSMutableArray")) void
#<<c-code
   [___arg1 addObject:___arg2];
c-code
))

(define objc:NSMutableArray/add-dictionary!
  (c-lambda ((pointer "NSMutableArray") (pointer "NSMutableDictionary")) void
#<<c-code
   [___arg1 addObject:___arg2];
c-code
))

(define (objc:NSMutableArray/add-value! arr val)
  (let ((v (bard->objc val)))
    (cond
     ((boolean? val) (objc:NSMutableArray/add-number! arr v))
     ((integer? val) (objc:NSMutableArray/add-number! arr v))
     ((flonum? val) (objc:NSMutableArray/add-number! arr v))
     ((string? val) (objc:NSMutableArray/add-string! arr v))
     ((symbol? val) (objc:NSMutableArray/add-string! arr v))
     ((keyword? val) (objc:NSMutableArray/add-string! arr v))
     ((%list? val) (objc:NSMutableArray/add-array! arr v))
     ;;((%frame? val) (objc:NSMutableArray/add-dictionary! arr v))
     (else (begin
             (display "List error: can't add ")
             (display (object->string val))
             (display " to NSMutableArray"))))))

;;; frames

(define objc:make-NSMutableDictionary
  (c-lambda () (pointer "NSMutableDictionary")
#<<c-code
   NSMutableDictionary* dict = [NSMutableDictionary dictionary];
   ___result_voidstar = (void*)dict;
c-code
))

(define objc:NSMutableDictionary/put-number-at-key!
  (c-lambda ((pointer "NSMutableDictionary") (pointer "NSString")(pointer "NSNumber")) void
#<<c-code
   [___arg1 setObject:___arg3 forKey:___arg2];
c-code
))

(define objc:NSMutableDictionary/put-string-at-key!
  (c-lambda ((pointer "NSMutableDictionary") (pointer "NSString")(pointer "NSString")) void
#<<c-code
   [___arg1 setObject:___arg3 forKey:___arg2];
c-code
))

(define objc:NSMutableDictionary/put-array-at-key!
  (c-lambda ((pointer "NSMutableDictionary") (pointer "NSString")(pointer "NSMutableArray")) void
#<<c-code
   [___arg1 setObject:___arg3 forKey:___arg2];
c-code
))

(define objc:NSMutableDictionary/put-dictionary-at-key!
  (c-lambda ((pointer "NSMutableDictionary") (pointer "NSString")(pointer "NSMutableDictionary")) void
#<<c-code
   [___arg1 setObject:___arg3 forKey:___arg2];
c-code
))

(define (objc:list->NSMutableArray items)
  (let* ((arr (objc:make-NSMutableArray)))
    (for-each (lambda (it)(objc:NSMutableArray/add-value! arr it))
              items)
    arr))

(define (objc:ralist->NSMutableArray items)
  (let* ((arr (objc:make-NSMutableArray)))
    (let loop ((items items))
      (if (not (%null? items))
          (begin
            (objc:NSMutableArray/add-value! arr (%car items))
            (loop (%cdr items)))))
    arr))

(define (objc:frame->NSMutableDictionary fr)
  (let* ((dict (objc:make-NSMutableDictionary))
         (keys (%frame-keys fr)))
    (%for-each (lambda (key)
                 (let* ((k (bard->objc key))
                        (val (%frame-get fr key #f))
                        (v (bard->objc val)))
                   (newline)
                   (display "objc:frame->NSMutableDictionary: ")
                   (display (object->string val))
                   (cond
                    ((boolean? val) (objc:NSMutableDictionary/put-number-at-key! dict k v))
                    ((integer? val) (objc:NSMutableDictionary/put-number-at-key! dict k v))
                    ((flonum? val) (objc:NSMutableDictionary/put-number-at-key! dict k v))
                    ((string? val) (objc:NSMutableDictionary/put-string-at-key! dict k v))
                    ((symbol? val) (objc:NSMutableDictionary/put-string-at-key! dict k v))
                    ((keyword? val) (objc:NSMutableDictionary/put-string-at-key! dict k v))
                    ((%list? val) (objc:NSMutableDictionary/put-array-at-key! (objc:ralist->NSMutableArray val)))
                    ;;((%frame? val) )
                    (else (begin
                            (display "Frame error: can't add ")
                            (display (object->string val))
                            (display " to NSMutableDictionary"))))))
               keys)
    (newline)
    (display "Done with objc:frame->NSMutableDictionary")
    dict))

(define (bard->objc val)
  (newline)
  (display "bard->objc: ")
  (display (object->string val))
  (cond
   ((boolean? val)(objc:boolean->NSNumber val))
   ((integer? val)(objc:integer->NSNumber val))
   ((flonum? val)(objc:float->NSNumber val))
   ((string? val)(objc:string->NSString val))
   ((symbol? val)(objc:string->NSString (symbol->string val)))
   ((keyword? val)(objc:string->NSString (keyword->string val)))
   ((%list? val)(objc:ralist->NSMutableArray val))
   ;;((%frame? val)(objc:frame->NSMutableDictionary val))
   (else (begin
           (newline)
           (display "Conversion error: can't convert ")
           (display (object->string val))
           (display " to Objective-C")
           #f))))


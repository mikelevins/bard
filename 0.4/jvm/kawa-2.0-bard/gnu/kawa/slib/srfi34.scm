(module-compile-options warn-undefined-variable: #t
			warn-invoke-unknown-method: #t)
(module-export with-exception-handler guard raise)
(provide 'srfi-34)

(define-simple-class <raise-object-exception> (<java.lang.Throwable>)
  (value)
  ((*init* value)
   (set! (*:.value (this)) value)))

(define (with-exception-handler handler thunk)
  (try-catch
   (thunk)
   (ex <raise-object-exception> (handler (*:.value ex)))
   (ex <java.lang.Throwable> (handler ex))))

(define (raise obj)
  (primitive-throw (make <raise-object-exception> obj)))

(define-syntax guard
  (syntax-rules ()
    ((guard (var . clauses) . body)
     (try-catch
      (begin . body)
      (ex <java.lang.Throwable>
	  (let ((var
		 (if (instance? ex <raise-object-exception>)
		     (field (as <raise-object-exception> ex) 'value)
		     ex)))
	    (guard-aux (primitive-throw ex) . clauses)))))))

;; The implementation of the guard-aux macro is from the SRFI-34
;; reference implementation which is:
;; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.

;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.

;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.

;; This document and the information contained herein is provided on
;; an "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE
;; ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS
;; FOR A PARTICULAR PURPOSE.

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp 
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     test)
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

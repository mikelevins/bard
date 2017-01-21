;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          error_macros.scm
;;;; Project:       Bard
;;;; Purpose:       managing Scheme error reports
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $ERR:NO-ERROR 0)
(define $ERR:GENERAL-EXCEPTION 1)
(define $ERR:HEAP-OVERFLOW 2)
(define $ERR:STACK-OVERFLOW 3)
(define $ERR:OS-EXCEPTION 4)
(define $ERR:UNDEFINED-ENVIRONMENT-VARIABLE 5)
(define $ERR:NO-SUCH-FILE 6)
(define $ERR:SCHEDULER-EXCEPTION 7)
(define $ERR:DEADLOCK-DETECTED 8)
(define $ERR:ABANDONED-MUTEX 9)
(define $ERR:JOIN-TIMEOUT 10)
(define $ERR:THREAD-ALREADY-STARTED 11)
(define $ERR:THREAD-TERMINATED 12)
(define $ERR:UNCAUGHT-EXEPTION 13)
(define $ERR:ERROR-CONVERTING-TO-BARD 14)
(define $ERR:ERROR-CONVERTING-TO-C 15)
(define $ERR:ERROR-RETURNING-FROM-C 16)
(define $ERR:BAD-DATA-FORMAT 17)
(define $ERR:EXPRESSION-SYNTAX-ERROR 18)
(define $ERR:UNDEFINED-GLOBAL 19)
(define $ERR:WRONG-TYPE 20)
(define $ERR:OUT-OF-RANGE 21)
(define $ERR:DIVIDE-BY-ZERO 22)
(define $ERR:IMPROPER-LIST-LENGTH 23)
(define $ERR:WRONG-NUMBER-OF-ARGUMENTS 24)
(define $ERR:TOO-MANY-ARGUMENTS 25)
(define $ERR:OPERATOR-NOT-A-PROCEDURE 26)
(define $ERR:UNKNOWN-KEYWORD-ARGUMENT 27)
(define $ERR:KEYWORD-EXPECTED 28)
(define $ERR:NONCONTINUABLE-EXCEPTION 29)
(define $ERR:UNBOUND-TABLE-KEY 30)
(define $ERR:UNIDENTIFIED-ERROR 127)

(define (error->error-code err)
  (cond
   ((error-exception? err) $ERR:GENERAL-EXCEPTION)
   ((heap-overflow-exception? err) $ERR:HEAP-OVERFLOW)
   ((stack-overflow-exception? err) $ERR:STACK-OVERFLOW)
   ((os-exception? err) $ERR:OS-EXCEPTION)
   ((unbound-os-environment-variable-exception? err) $ERR:UNDEFINED-ENVIRONMENT-VARIABLE)
   ((no-such-file-or-directory-exception? err) $ERR:NO-SUCH-FILE)
   ((scheduler-exception? err) $ERR:SCHEDULER-EXCEPTION)
   ((deadlock-exception? err) $ERR:DEADLOCK-DETECTED)
   ((abandoned-mutex-exception? err) $ERR:ABANDONED-MUTEX)
   ((join-timeout-exception? err) $ERR:JOIN-TIMEOUT)
   ((started-thread-exception? err) $ERR:THREAD-ALREADY-STARTED)
   ((terminated-thread-exception? err) $ERR:THREAD-TERMINATED)
   ((uncaught-exception? err) $ERR:UNCAUGHT-EXEPTION)
   ((cfun-conversion-exception? err) $ERR:ERROR-CONVERTING-TO-BARD)
   ((sfun-conversion-exception? err) $ERR:ERROR-CONVERTING-TO-C)
   ((multiple-c-return-exception? err) $ERR:ERROR-RETURNING-FROM-C)
   ((datum-parsing-exception? err) $ERR:BAD-DATA-FORMAT)
   ((expression-parsing-exception? err) $ERR:EXPRESSION-SYNTAX-ERROR)
   ((unbound-global-exception? err) $ERR:UNDEFINED-GLOBAL)
   ((type-exception? err) $ERR:WRONG-TYPE)
   ((range-exception? err) $ERR:OUT-OF-RANGE)
   ((divide-by-zero-exception? err) $ERR:DIVIDE-BY-ZERO)
   ((improper-length-list-exception? err) $ERR:IMPROPER-LIST-LENGTH)
   ((wrong-number-of-arguments-exception? err) $ERR:WRONG-NUMBER-OF-ARGUMENTS)
   ((number-of-arguments-limit-exception? err) $ERR:TOO-MANY-ARGUMENTS)
   ((nonprocedure-operator-exception? err) $ERR:OPERATOR-NOT-A-PROCEDURE)
   ((unknown-keyword-argument-exception? err) $ERR:UNKNOWN-KEYWORD-ARGUMENT)
   ((keyword-expected-exception? err) $ERR:KEYWORD-EXPECTED)
   ((noncontinuable-exception? err) $ERR:NONCONTINUABLE-EXCEPTION)
   ((unbound-table-key-exception? err) $ERR:UNBOUND-TABLE-KEY)
   (else $ERR:UNIDENTIFIED-ERROR)))

(define $bard-errors)

(define-type error-report
  constructor: %private-make-error-report
  error-code
  error-message)

(define (make-error-report an-error)
  (let ((msg (error->string an-error))
        (code (error->error-code an-error)))
    (%private-make-error-report code msg)))

(define (clear-last-error-report!)
  (if (not (null? $bard-errors))
      (set! $bard-errors (cdr $bard-errors))))

(define (clear-error-reports!)
  (set! $bard-errors '()))

(define (record-error-report! report)
  (set! $bard-errors (cons report $bard-errors)))

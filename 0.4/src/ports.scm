;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ports.scm
;;;; Project:       Bard
;;;; Purpose:       port customizations for Bard
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(define (default-prompter port)
  (let ((state (input-port-read-state port)))
    (if (char=? state #\Newline)
	""
	(if (char=? state #\Space)
            "bard> "
            "    > "))))

(define (write-simple value #!optional (out ::output-port (current-output-port))) ::void
  ;;(gnu.kawa.functions.DisplayFormat:schemeWriteSimpleFormat:format value out)
  (net.bardcode.Print:bardWriteSimpleFormat:format value out))

(define-private (%write-shared% fmt::gnu.kawa.functions.DisplayFormat
                                value
                                out::output-port) ::void
                 (let ((pretty-out (out:getPrettyWriter)))
                   (pretty-out:initialiseIDHash)
                   (pretty-out:setSharing #t)
                   (try-finally
                    (fmt:format value out)
                    (pretty-out:setSharing #f))
                   (pretty-out:clearIDHash)
                   (pretty-out:writeEndOfExpression)
                   (pretty-out:resolveBackReferences)
                   (pretty-out:flush)))

(define (write
	 value #!optional (out ::output-port (current-output-port))) ::void
         (%write-shared%
          (if (eqv? *print-circle* #t)
              ;;gnu.kawa.functions.DisplayFormat:schemeWriteSharedFormat
              net.bardcode.Print:bardWriteSharedFormat
              ;;gnu.kawa.functions.DisplayFormat:schemeWriteFormat
              net.bardcode.Print:bardWriteFormat)
          value out))

(define (write-shared
	 value #!optional (out ::output-port (current-output-port))) ::void
         (%write-shared%
          ;;gnu.kawa.functions.DisplayFormat:schemeWriteSharedFormat
          net.bardcode.Print:bardWriteSharedFormat
          value out))

(define (write-with-shared-structure 
	 value #!optional (out ::output-port (current-output-port))) ::void
         (%write-shared%
          ;;gnu.kawa.functions.DisplayFormat:schemeWriteSharedFormat
          net.bardcode.Print:bardWriteSharedFormat
          value out))

(define (display value #!optional (out (current-output-port))) :: <void>
  ;;(*:format gnu.kawa.functions.DisplayFormat:schemeDisplayFormat value out)
  (*:format net.bardcode.Print:bardDisplayFormat value out))


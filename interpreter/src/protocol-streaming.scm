;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-streaming.scm
;;;; Project:       Bard
;;;; Purpose:       taking values from streams and putting values on streams
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")

(define bard:contents
  (make-function debug-name: 'contents
                 signatures: (list (signature (InputStream) #f (List)))))

;;; TODO: add port-kind handling once I figure out how to reliably
;;;       distinguish port kinds
(define (%bard-iostream-contents stream)
  (if (input-port? stream)
      (read-all stream read-char)
      (error (str "Not an input stream:" (%as-string stream)))))

(%add-primitive-method! bard:contents
                        (list <iostream>)
                        %bard-iostream-contents
                        debug-name: 'contents)

(define bard:lines
  (make-function debug-name: 'lines
                 signatures: (list (signature (InputStream) #f (List)))))

;;; TODO: add port-kind handling once I figure out how to reliably
;;;       distinguish port kinds
(define (%bard-iostream-lines stream)
  (if (input-port? stream)
      (read-all stream read-line)
      (error (str "Not an input stream:" (%as-string stream)))))

(%add-primitive-method! bard:lines
                        (list <iostream>)
                        %bard-iostream-lines
                        debug-name: 'lines)

(define bard:put
  (make-function debug-name: 'put
                 signatures: (list (signature (Anything OutputStream) #f (Boolean)))))

(define bard:stream-direction
  (make-function debug-name: 'stream-direction
                 signatures: (list (signature (Stream) #f (IODirection)))))

(define (%bard-stream-direction stream)
  (if (input-port? stream)
      'input
      (if (output-port? stream)
          'output
          (error (str "Not an iostream:" (%as-string stream))))))

(%add-primitive-method! bard:stream-direction
                        (list <iostream>)
                        %bard-stream-direction
                        debug-name: 'stream-direction)


(define bard:stream-mode
  (make-function debug-name: 'stream-mode
                 signatures: (list (signature (Stream) #f (IOMode)))))

(define bard:stream-type
  (make-function debug-name: 'stream-type
                 signatures: (list (signature (Stream) #f (IOType)))))

;;; take n stream -- see protocol-listing for function definition
;;; take-one stream -- see protocol-listing for function definition
;;; vals stream -- see protocol-mapping for function definition


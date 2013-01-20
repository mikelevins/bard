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

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

(define-protocol-function Streaming contents
  signatures: (list (signature (InputStream) #f (List))))

(define (%bard-iostream-contents stream)
  (if (input-port? stream)
      (read-all stream read-char)
      (error (str "Not an input stream:" (%as-string stream)))))

(define-primitive-method contents
  (list <iostream>)
  %bard-iostream-contents)

(define-protocol-function Streaming lines
  signatures: (list (signature (InputStream) #f (List))))

(define (%bard-iostream-lines stream)
  (if (input-port? stream)
      (read-all stream read-line)
      (error (str "Not an input stream:" (%as-string stream)))))

(define-primitive-method lines
  (list <iostream>)
  %bard-iostream-lines)

(define-protocol-function Streaming put
  signatures: (list (signature (Anything OutputStream) #f (Boolean))))

(define-protocol-function Streaming stream-direction
  signatures: (list (signature (Stream) #f (IODirection))))

(define (%bard-stream-direction stream)
  (if (input-port? stream)
      'input
      (if (output-port? stream)
          'output
          (error (str "Not an iostream:" (%as-string stream))))))

(define-primitive-method stream-direction
  (list <iostream>)
  %bard-stream-direction)


(define-protocol-function Streaming stream-mode
  signatures: (list (signature (Stream) #f (IOMode))))

(define-protocol-function Streaming stream-type
  signatures: (list (signature (Stream) #f (IOType))))



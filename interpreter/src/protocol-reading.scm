;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-reading.scm
;;;; Project:       Bard
;;;; Purpose:       pulling values from input streams
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:current-input
  (make-primitive
   procedure: current-input-port
   debug-name: 'current-input
   required-count: 0
   restarg: #f))

(define bard:read-line
  (make-primitive
   procedure: read-line
   debug-name: 'read-line
   required-count: 1
   restarg: #f))

(define (%bard-read-lines in)
  (cond
   ((input-port? in)(read-all in read-line))
   ((string? in)(call-with-input-string in (lambda (stream)(read-all stream read-line))))
   (else (error (string-append "Invalid argument to read-lines: "
                               (object->string in))))))

(define bard:read-lines
  (make-primitive
   procedure: %bard-read-lines
   debug-name: 'read-lines
   required-count: 1
   restarg: #f))




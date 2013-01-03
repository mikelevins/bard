;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-writing.scm
;;;; Project:       Bard
;;;; Purpose:       pushing values onto output streams
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:current-output
  (make-primitive
   procedure: current-output-port
   debug-name: 'current-output
   required-count: 0
   restarg: #f))

(define bard:display
  (make-primitive
   procedure: display
   debug-name: 'display
   required-count: 1
   restarg: #f))

(define (%bard-write data out)
  (let ((data (if (string? data)
                  data
                  (%as-string data))))
    (cond
     ((output-port? out)(write-substring data 0 (string-length data) out))
     ((string? out)(call-with-output-file out 
                     (lambda (out)
                       (write-substring data 0 (string-length data) out))))
     (else (error (string-append "Invalid output argument to write: "
                                 (object->string out)))))))

(define bard:write
  (make-primitive
   procedure: %bard-write
   debug-name: 'write
   required-count: 2
   restarg: #f))



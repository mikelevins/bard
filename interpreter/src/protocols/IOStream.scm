;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          IOStream.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the IOStream protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol IOStream)

;;; close
;;; ---------------------------------------------------------------------
;;; (close stream)

(define bard:close close-input-port)

;;; current-input
;;; ---------------------------------------------------------------------

(define bard:current-input current-input-port)

;;; current-output
;;; ---------------------------------------------------------------------

(define bard:current-output current-output-port)

;;; display
;;; ---------------------------------------------------------------------

(define bard:display display)

;;; input-stream?
;;; ---------------------------------------------------------------------

(define bard:input-stream? input-port?)

;;; iostream?
;;; ---------------------------------------------------------------------

(define (bard:iostream? x) (or (input-port? x)(output-port? x)))

;;; load
;;; ---------------------------------------------------------------------
;;; (load pathname)

(define bard:load (%make-function name: 'load))

(%function-add-method! bard:load `(,<string>)
                       (%primitive-method (path)
                                          (newline)
                                          (display (string-append "Loading " path "..."))
                                          (newline)
                                          (call-with-input-file path
                                            (lambda (in)
                                              (let loop ((form (bard:read in)))
                                                (if (eqv? form #!eof)
                                                    (newline)
                                                    (begin
                                                      (newline)
                                                      (display (%as-string (%eval form (%null-environment))))
                                                      (loop (bard:read in)))))))))


;;; open
;;; ---------------------------------------------------------------------

(define bard:open
  (%primitive-method (& args)
                     (if (null? args)
                         (error "no argument supplied to open.")
                         (let ((arg (car args)))
                           (if (string? arg)
                               (open-file arg)
                               (let ((settings (%to-type <frame> arg)))
                                 (if (%frame? settings)
                                     (let ((path (%frame-get settings path: #f)))
                                       (if path
                                           (let ((direction (%frame-get settings direction: 'input)))
                                             (case direction
                                               ((input) (open-input-file path))
                                               ((output) (open-output-file path))
                                               (else (error (string-append "open called with no path argument")))))
                                           (error "no path argument supplied to open")))
                                     (error (string-append "wrong type of argument to open: "
                                                           (%as-string arg))))))))))


;;; output-stream?
;;; ---------------------------------------------------------------------

(define bard:output-stream? output-port?)

;;; print
;;; ---------------------------------------------------------------------
;;; defined in src/print.scm


;;; read
;;; ---------------------------------------------------------------------
;;; defined in src/reader/read.scm


;;; read-file
;;; ---------------------------------------------------------------------
;;; (read-file pathname)

(define bard:read-file (%make-function name: 'read-file))

(%function-add-method! bard:read-file `(,<string>)
                       (%primitive-method (path)
                                          (newline)
                                          (call-with-input-file path
                                            (lambda (in)
                                              (let loop ((line (read-line in))
                                                         (result ""))
                                                (if (eqv? line #!eof)
                                                    result
                                                    (loop (read-line in) (string-append result line (string #\newline)))))))))


;;; read-line
;;; ---------------------------------------------------------------------
;;; (read-line &optional (stream (current-input)))

(define bard:read-line (%make-function name: 'read-line))

(%function-add-method! bard:read-line `(& args)
                       (%primitive-method (& args)
                                          (let ((in (if (null? args)
                                                        (current-input-port)
                                                        in)))
                                            (read-line in))))

;;; read-lines
;;; ---------------------------------------------------------------------
;;; (read-lines pathname)

(define bard:read-lines (%make-function name: 'read-lines))

(%function-add-method! bard:read-lines `(,<string>)
                       (%primitive-method (path)
                                          (newline)
                                          (call-with-input-file path
                                            (lambda (in)
                                              (let loop ((line (read-line in))
                                                         (result '()))
                                                (if (eqv? line #!eof)
                                                    (reverse result)
                                                    (loop (read-line in) (cons line result))))))))

;;; show
;;; ---------------------------------------------------------------------
;;; (show value) => Text

(define bard:show %as-string)

;;; write
;;; ---------------------------------------------------------------------
;;; (write value &optional (stream (current-output)))

(define bard:write (%make-function name: 'write))

(%function-add-method! bard:write `(,Anything & args)
                       (%primitive-method (val & args)
                                          (let ((out (if (null? args)
                                                         (current-output-port)
                                                         (car args))))
                                            (write (%as-string val) out))))

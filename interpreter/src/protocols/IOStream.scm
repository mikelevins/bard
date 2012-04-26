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

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol IOStream)

;;; close
;;; ---------------------------------------------------------------------
;;; (close stream)

(define bard:close (%make-function name: 'close))

(%function-add-method! bard:close `(,<input-stream>) (lambda (s)(close-input-port s)))
(%function-add-method! bard:close `(,<output-stream>) (lambda (s)(close-output-port s)))

;;; current-input
;;; ---------------------------------------------------------------------

(define (bard:current-input)(current-input-port))

;;; current-output
;;; ---------------------------------------------------------------------

(define (bard:current-output)(current-output-port))

;;; display
;;; ---------------------------------------------------------------------
;;; (display value &optional (stream (current-output)))

(define bard:display (%make-function name: 'display))

;;; input-stream?
;;; ---------------------------------------------------------------------

(define bard:input-stream? (%make-function name: 'input-stream?))

(%function-add-method! bard:input-stream? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:input-stream? `(,<input-stream>) (lambda (x)(%true)))

;;; iostream?
;;; ---------------------------------------------------------------------

(define bard:iostream? (%make-function name: 'iostream?))

(%function-add-method! bard:iostream? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:iostream? `(,<input-stream>) (lambda (x)(%true)))
(%function-add-method! bard:iostream? `(,<output-stream>) (lambda (x)(%true)))

;;; load
;;; ---------------------------------------------------------------------
;;; (load pathname)

(define bard:load (%make-function name: 'load))


;;; open
;;; ---------------------------------------------------------------------
;;; (open pathname &optional (settings {:direction 'input :mode 'text}))

(define bard:open (%make-function name: 'open))


;;; output-stream?
;;; ---------------------------------------------------------------------

(define bard:output-stream? (%make-function name: 'output-stream?))

(%function-add-method! bard:output-stream? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:output-stream? `(,<output-stream>) (lambda (x)(%true)))

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

;;; read-line
;;; ---------------------------------------------------------------------
;;; (read-line &optional (stream (current-input)))

(define bard:read-line (%make-function name: 'read-line))


;;; read-lines
;;; ---------------------------------------------------------------------
;;; (read-lines pathname)

(define bard:read-lines (%make-function name: 'read-lines))


;;; show
;;; ---------------------------------------------------------------------
;;; (show value) => Text

(define bard:show (%make-function name: 'show))

;;; write
;;; ---------------------------------------------------------------------
;;; (write value &optional (stream (current-output)))

(define bard:write (%make-function name: 'write))


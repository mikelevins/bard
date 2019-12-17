;;;; ***********************************************************************
;;;;
;;;; Name:          types-base.scm
;;;; Project:       Bard
;;;; Purpose:       base Bard protocols
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base structs
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <protocol>
;;; ----------------------------------------------------------------------

(define tags:$bard-protocol (%next-bard-type-number))
(define <protocol> (make-base-struct '<protocol> tags:$bard-protocol))

;;; constructor

(define (%make-protocol name)(make-protocol-instance <protocol> name (make-table test: eq?)))

;;; accessors

(define (%protocol-ref p fn-name)
  (table-ref (protocol-instance-functions p) fn-name #f))

(define (%protocol-add! p fn-name fn)
  (table-set! (protocol-instance-functions p) fn-name fn)
  p)

(define (%protocol-remove! p fn-name)
  (table-set! (protocol-instance-functions p) fn-name)
  p)

;;; ----------------------------------------------------------------------
;;; protocol registry
;;; ----------------------------------------------------------------------
;;; used to enable us to tell which structs belong to which roles,
;;; and also for documentation purposes

(define +protocols+ (make-table test: eq?))

(define (register-protocol pname proto)
  (table-set! +protocols+ pname proto))

(define (get-protocol pname)
  (table-ref +protocols+ pname #f))

(define (protocol-roles p)
  (let ((functions (protocol-instance-functions p))
        (role-list '()))
    (table-for-each (lambda (fname fn)
                      (let* ((sigs (function-signatures fn))
                             (input-types (apply append (map signature-input-types sigs)))
                             (roles (filter role-instance? input-types)))
                        (set! role-list (append role-list roles))))
                    functions)
    (nub role-list)))



;;; definitions of protocols
;;; ----------------------------------------------------------------------
;;; convention: protocol names are present participles

(define Addressing     (%make-protocol 'Addressing))     ; locating resources
(register-protocol 'Addressing Addressing)
(define Applying       (%make-protocol 'Applying))       ; applying function-like values
(register-protocol 'Applying Applying)
(define Calculating    (%make-protocol 'Calculating))    ; performing arithmetic and other calculating tasks
(register-protocol 'Calculating Calculating)
(define Comparing      (%make-protocol 'Comparing))      ; comparing values for equality
(register-protocol 'Comparing Comparing)
(define Constructing   (%make-protocol 'Constructing))   ; constructing values
(register-protocol 'Constructing Constructing)
(define Converting     (%make-protocol 'Converting))     ; producing values of one type based on inputs of another
(register-protocol 'Converting Converting)
(define Creating     (%make-protocol 'Creating))         ; constructing values
(register-protocol 'Creating Creating)
(define Listing        (%make-protocol 'Listing))        ; arranging values in lists
(register-protocol 'Listing Listing)
(define Mapping        (%make-protocol 'Mapping))        ; arranging values in tables
(register-protocol 'Mapping Mapping)
(define Ordering       (%make-protocol 'Ordering))       ; arranging values by magnitude
(register-protocol 'Ordering Ordering)
(define Pairing        (%make-protocol 'Pairing))        ; arranging values in pairs
(register-protocol 'Pairing Pairing)
(define Reading        (%make-protocol 'Reading))        ; getting values from input streams
(register-protocol 'Reading Reading)
(define Streaming      (%make-protocol 'Streaming))      ; data sources and sinks
(register-protocol 'Streaming Streaming)
(define System         (%make-protocol 'System))         ; system tools
(register-protocol 'System System)
(define TextProcessing (%make-protocol 'TextProcessing)) ; processing text
(register-protocol 'TextProcessing TextProcessing)
(define Typing         (%make-protocol 'Typing))         ; discriminating values by type
(register-protocol 'Typing Typing)
(define Writing        (%make-protocol 'Writing))        ; putting values into output streams
(register-protocol 'Writing Writing)


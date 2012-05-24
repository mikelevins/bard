;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       representation of basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-macros.scm")

;;; ----------------------------------------------------------------------
;;; undefined
;;; ----------------------------------------------------------------------

(define (%undefined) #!unbound)
(define (%undefined? x) (eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

;;; ---------------------------------------------------------------------
;;; primitive-procedure
;;; ---------------------------------------------------------------------

(define %primitive-procedure? procedure?)

;;; ----------------------------------------------------------------------
;;; nothing
;;; ----------------------------------------------------------------------

(define (%nothing) '())
(define %nothing? null?)
(define (%something? x)(not (%nothing? x)))

;;; ----------------------------------------------------------------------
;;; characters
;;; ----------------------------------------------------------------------

(define %character? char?)

;;; ----------------------------------------------------------------------
;;; boolean
;;; ----------------------------------------------------------------------

(define (%false) #f)

(define (%false? x) 
  (or (eqv? x (%false))
      (%nothing? x)))

(define (%true) #t)
(define (%true? x) (not (%false? x)))

(define (%boolean? x)
  (or (eqv? x (%false))
      (eqv? x (%true))))

;;; ----------------------------------------------------------------------
;;; numbers
;;; ----------------------------------------------------------------------

(define %fixnum? ##fixnum?)
(define %fixnum? ##bignum?)

(define (%integer? x)
  (or (##fixnum? x)
      (##bignum? x)))

(define (%float? x)
  (##flonum? x))

(define (%ratio? x)
  (##ratnum? x))

(define (%number? x) 
  (or (%integer? x)
      (%float? x)
      (%ratio? x)))

;;; ----------------------------------------------------------------------
;;; names
;;; ----------------------------------------------------------------------

(define %symbol? symbol?)
(define %keyword? keyword?)
(define (%name? x) 
  (or (%symbol? x)
      (%keyword? x)))

;;; ----------------------------------------------------------------------
;;; text
;;; ----------------------------------------------------------------------

(define %string? string?)
(define %text? string?)

;;; ---------------------------------------------------------------------
;;; list
;;; ---------------------------------------------------------------------

(define %nil ra:null)
(define %null? ra:null?)
(define %list? ra:list?)
(define %cons ra:cons)
(define %car ra:car)
(define %cdr ra:cdr)
(define %list ra:list)
(define %make-list ra:make-list)
(define %length ra:length)
(define %append ra:append)
(define %reverse ra:reverse)

(define (%drop n ls)(ra:list-tail ls n))

(define (%remove x ls #!optional (test equal?))
  (let loop ((items ls)
             (result %nil))
    (if (%null? items)
        result
        (let ((item (%car items)))
          (if (test x item)
              (loop (%cdr items) result)
              (loop (%cdr items)(%append result (%list item))))))))

(define %list-ref ra:list-ref)
(define (%list-put ls n val) (ra:list-set ls n val))
(define %map ra:map)
(define %for-each ra:for-each)
(define %ralist->cons ra:random-access-list->linear-access-list)
(define %cons->ralist ra:linear-access-list->random-access-list)

(%define-structure-type <ralist> (##structure-type (%list 0)) %list?)

;;; ---------------------------------------------------------------------
;;; frame
;;; ---------------------------------------------------------------------
;;; frames based on Adams' weight-balanced binary trees Bard frames
;;; are obliged to provide a stable order for keys, in order to
;;; support the List protocol.  we do that by representing a frame as
;;; a structure with a wttree for the slots, and an ralist that
;;; presents the keys in the order they were added


;;; wttrees rely on an ordering function to dispose keys.
;;; %key< orders any two Bard values that may be used as keys
;;; any Bard value except nothing or undefined may be a key

(define (%key< k1 k2)
  (< (object->serial-number k1)
     (object->serial-number k2)))

(define $slots-wt-type (make-wt-tree-type %key<))
(define $empty-slots (make-wt-tree $slots-wt-type))

(define-type %frame
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  constructor: %private-make-frame
  (slots %frame-slots)
  (keys %frame-keys))

(%define-structure-type <frame> (##structure-type (%private-make-frame $empty-slots (%list))) %frame?)

(define (%make-frame kv-plist)
  (let* ((alist (plist->alist kv-plist))
         (slots (alist->wt-tree $slots-wt-type alist))
         (keys (%cons->ralist (map car alist))))
    (%private-make-frame slots keys)))

(define (%frame . kv-plist)(%make-frame kv-plist))

(define (%frame-get fr key #!optional (default (%nothing)))
  (wt-tree/lookup (%frame-slots fr) key default))

(define (%frame-put fr key value)
  (let* ((new-slots (wt-tree/add (wt-tree/delete (%frame-slots fr) key) key value))
         (new-keys (%append (%remove key (%frame-keys fr))(%list key))))
    (%private-make-frame new-slots new-keys)))


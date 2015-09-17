(module-name <kawa.lib.rnrs.hashtables>)
(export make-eq-hashtable make-eqv-hashtable make-hashtable
	hashtable? hashtable-size
	hashtable-ref hashtable-set! hashtable-delete!
	hashtable-contains? hashtable-update!
	hashtable-copy hashtable-clear!
	hashtable-keys hashtable-entries
	hashtable-equivalence-function hashtable-hash-function
	hashtable-mutable?
	equal-hash string-hash string-ci-hash symbol-hash)
(require <kawa.lib.kawa.hashtable>)
;(import (kawa hashable))

;; Not exported
(define (hash-by-identity obj) :: int
  (java.lang.System:identityHashCode obj))
;; FIXME is supposed to always terminate, but that necessarily the case.
(define (hash-for-eqv obj) :: int
  (obj:hashCode))

(define (make-eq-hashtable
	 #!optional (k :: int hashtable:DEFAULT_INITIAL_SIZE))
  :: hashtable
  (make hashtable eq? hash-by-identity hashtable:DEFAULT_INITIAL_SIZE))

(define (make-eqv-hashtable
	 #!optional (k :: int hashtable:DEFAULT_INITIAL_SIZE))
  :: hashtable
  (make hashtable eqv? hash-for-eqv hashtable:DEFAULT_INITIAL_SIZE))

(define (make-hashtable
	 (hash-function :: <procedure>)
	 (comparison :: <procedure>)
	 #!optional (size :: <int> hashtable:DEFAULT_INITIAL_SIZE))
  :: hashtable
  (make hashtable comparison hash-function size))

(define (hashtable? obj) :: <boolean>
  (instance? obj hashtable))

(define (hashtable-size (ht :: hashtable)) :: int
  (ht:size))

(define (hashtable-ref (ht :: hashtable) key default)
  (let ((node (ht:getNode key)))
    (if (eq? node #!null)
	default
	(*:getValue node))))

(define (hashtable-set! (ht :: hashtable) key value) :: void
  (hashtable-check-mutable  ht)
  (ht:put key value))

(define (hashtable-delete! (ht :: hashtable) key) :: void
  (hashtable-check-mutable  ht)
  (ht:remove key))

(define (hashtable-contains? (ht :: hashtable) key) :: boolean
  (not (eq? (ht:getNode key) #!null)))

(define (hashtable-update! (ht :: hashtable) key
			   (proc  :: procedure) default)
  (hashtable-check-mutable  ht)
  (let ((node (ht:getNode key)))
    (if (eq? node #!null)
	(hashtable-set! ht key (proc default))
	(node:setValue (proc (node:getValue))))))

(define (hashtable-copy (ht :: hashtable) #!optional (mutable :: boolean #f))
  :: hashtable
   (make hashtable ht mutable))

(define (hashtable-clear!
	 (ht :: hashtable)
	 #!optional (k :: int 64))
  :: void
  (hashtable-check-mutable  ht)
  (ht:clear))

(define (hashtable-keys (ht :: hashtable)) :: vector
  (ht:keysVector))

(define (hashtable-entries (ht :: hashtable))
  (let ((pair (ht:entriesVectorPair)))
    (values (car pair) (cdr pair))))

(define (hashtable-equivalence-function  (ht :: hashtable)) :: procedure
  ht:equivalenceFunction)

(define (hashtable-hash-function  (ht :: hashtable))
  (let ((hasher (ht:hashFunction ht)))
    (if (or (eqv? hasher hash-by-identity)
	    (eqv? hasher hash-for-eqv))
	#f
	hasher)))

(define (hashtable-mutable? (ht :: hashtable)) :: boolean
  ht:mutable)

;; FIXME is supposed to always terminate, but that is not guaranteed.
(define (equal-hash key)
  (key:hashCode))

(define (string-hash (s :: <string>))
  (s:hashCode))

(define (string-ci-hash (s :: <string>))
  ;; It would probably be more efficient to incrementally calculate
  ;; the hashCode rather than calling String.toLowerCase, but it's
  ;; tricky if we want to get the corner cases right, and it's
  ;; probably not worthwhile unless string-ci-hash gets used a lot.
  (((s:toString):toLowerCase):hashCode))

(define (symbol-hash (s :: <symbol>))
  (s:hashCode))

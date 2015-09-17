; Copyright © Per Bothner (2005, 2009). All Rights Reserved.
; (for Kawa-specific modifications and optimizations).
; Copyright © Panu Kalliokoski (2005). All Rights Reserved.

; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation
; files (the Software), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify,
; merge, publish, distribute, sublicense, and/or sell copies of the
; Software, and to permit persons to whom the Software is furnished
; to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(module-compile-options warn-undefined-variable: #t
			warn-invoke-unknown-method: #t)
(provide 'srfi-69)
(provide 'hash-table)
(export make-hash-table hash-table? alist->hash-table 
	hash-table-equivalence-function hash-table-hash-function 
	hash-table-ref hash-table-ref/default hash-table-set!
	hash-table-delete! hash-table-exists?
	hash-table-update! hash-table-update!/default 
	hash-table-size hash-table-keys hash-table-values hash-table-walk
	hash-table-fold hash-table->alist hash-table-copy hash-table-merge! 
	hash string-hash string-ci-hash hash-by-identity)
(import (kawa hashtable))
(import  (rename (except (rnrs hashtables)
			 string-hash string-ci-hash symbol-hash)
		 (hashtable-delete! hash-table-delete!)
		 (hashtable-contains? hash-table-exists?)
		 (hashtable-set! hash-table-set!)
		 (hashtable? hash-table?)
		 (hashtable-size hash-table-size)))

(define *default-bound* #x100000000)

(define (string-hash (s :: <string>) #!optional (bound :: <integer> #!null))
  (let ((h :: <int> (*:hashCode s)))
    (if (eq? bound #!null) h (modulo h bound))))

(define (string-ci-hash s #!optional (bound :: <integer> #!null))
  ;; It would probably be more efficient to incrementally calculate
  ;; the hashCode rather than calling String.toLowerCase, but it's
  ;; tricky if we want to get the corner cases right, and it's
  ;; probably not worthwhile unless string-ci-hash gets used a lot.
  (let ((h :: <int> (*:hashCode (*:toLowerCase (*:toString s)))))
    (if (eq? bound #!null) h (modulo h bound))))

(define (hash obj #!optional (bound :: <integer> #!null))
  (let ((h :: <int> (if (eq? obj #!null) 0 (*:hashCode obj))))
    (if (eq? bound #!null) h (modulo h bound))))

(define (hash-by-identity obj #!optional (bound :: <integer> #!null))
  (let ((h :: <int> (java.lang.System:identityHashCode obj)))
    (if (eq? bound #!null) h (modulo h bound))))

(define *default-table-size* 64)

(define (hash-table-equivalence-function (hash-table :: hashtable))
  :: <procedure>
  (*:.equivalenceFunction hash-table))

(define (hash-table-hash-function (hash-table :: hashtable))
  :: <procedure>
  (*:.hashFunction hash-table))

(define (appropriate-hash-function-for comparison) :: <procedure>
  (or (and (eq? comparison eq?) hash-by-identity)
      (and (eq? comparison string=?) string-hash)
      (and (eq? comparison string-ci=?) string-ci-hash)
      hash))

;; Optimize to use plain <GeneralHashTable> when defaults comparison/hash
; since that is more efficient (no boxing/unboxing of hashes).  FIXME
(define (make-hash-table #!optional
			 (comparison :: <procedure> equal?)
			 (hash :: <procedure>
			       (appropriate-hash-function-for comparison))
			 (size :: <int> *default-table-size*))
  :: hashtable
  (make hashtable comparison hash size))

#|
;; Are these part of the specification? FIXME
(define (make-hash-table-maker comp hash)
  (lambda args (apply make-hash-table (cons comp (cons hash args)))))
(define make-symbol-hash-table
  (make-hash-table-maker eq? symbol-hash))
(define make-string-hash-table
  (make-hash-table-maker string=? string-hash))
(define make-string-ci-hash-table
  (make-hash-table-maker string-ci=? string-ci-hash))
(define make-integer-hash-table
  (make-hash-table-maker = modulo))
|#

(define (hash-table-ref (hash-table :: hashtable)
			key
			#!optional default)
  (let ((node (*:getNode hash-table key)))
    (if (eq? node #!null)
	(if default (default)
	    (error "hash-table-ref: no value associated with" key))
	(*:getValue node))))

(define (hash-table-ref/default (hash-table :: hashtable)
				key default)
  (*:get hash-table key default))

(define (hash-table-update! (hash-table :: hashtable)
			    key function #!optional thunk) :: <void>
  (hashtable-check-mutable hash-table)
  (let ((node (hash-table:getNode key)))
    (if (eq? node #!null)
	(if thunk
	    (hash-table-set! hash-table key (function (thunk)))
	    (error "hash-table-update!: no value exists for key" key))
	(*:setValue node (function (*:getValue node))))))

(define (hash-table-update!/default (hash-table :: hashtable) key function default) :: <void>
  (hashtable-check-mutable hash-table)
  (let ((node (hash-table:getNode key)))
    (if (eq? node #!null)
	(hash-table-set! hash-table key (function default))
	(*:setValue node (function (*:getValue node))))))

(define (hash-table-walk (hash-table :: hashtable)
			 (proc :: <procedure>))
  :: <void>
  (*:walk hash-table proc))
	
(define (hash-table-fold (hash-table :: hashtable)
			 (proc :: <procedure>)
			 acc)
  (*:fold hash-table proc acc))

(define (alist->hash-table alist
			   #!optional
			   (comparison equal?)
			   (hash (appropriate-hash-function-for comparison))
			   (size (max *default-table-size*
				      (* 2 (length alist)))))
  (let ((hash-table (make-hash-table comparison hash size)))
    (for-each
      (lambda (elem)
	(hash-table-update!/default
	  hash-table (car elem) (lambda (x) x) (cdr elem)))
      alist)
    hash-table))

(define (hash-table->alist (hash-table :: hashtable))
  (*:toAlist hash-table))

(define (hash-table-copy (hash-table :: hashtable)) :: hashtable
  (make hashtable hash-table #t))

(define (hash-table-merge! (hash-table1 :: hashtable)
			   (hash-table2 :: hashtable))
  :: <void>
  (*:putAll hash-table1 hash-table2)
  hash-table1)

(define (hash-table-keys (hash-table :: hashtable))
  (hash-table-fold hash-table (lambda (key val acc) (cons key acc)) '()))

(define (hash-table-values (hash-table :: hashtable))
  (hash-table-fold hash-table (lambda (key val acc) (cons val acc)) '()))

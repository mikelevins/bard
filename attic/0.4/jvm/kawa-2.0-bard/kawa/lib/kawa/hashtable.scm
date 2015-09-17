(module-name <kawa.lib.kawa.hashtable>)
(export hashtable hashtable-check-mutable)
(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.misc>)

(define-alias hashnode gnu.kawa.util.HashNode)

(define-syntax hash-table-walk%
  (syntax-rules ()
    ((hash-table-walk% hash-table node use-node)
     (let* ((table ::hashnode[] (*:.table hash-table))
	    (length table:length))
       (do ((i :: <int> (- length 1) (- i 1)))
	   ((< i 0) #!void)
	 (do ((node ::hashnode (table i)
		    (invoke hash-table 'getEntryNext node)))
	     ((eq? node #!null) #!void)
	   use-node))))))

(define-simple-class hashtable (<gnu.kawa.util.GeneralHashTable>)
  class-name: ".HashTable"
  (equivalenceFunction :: <procedure>)
  (hashFunction :: <procedure>)
  (mutable :: boolean init: #t) ;; Not checked by these methods.
  ((*init* (eq :: procedure) (h :: procedure) (sz :: int))
   (invoke-special gnu.kawa.util.GeneralHashTable (this) '*init* sz)
   (set! equivalenceFunction eq)
   (set! hashFunction h))
  ((*init* (eq :: procedure) (h :: procedure))
   (set! equivalenceFunction eq)
   (set! hashFunction h))
  ((*init* (ht :: hashtable) (mutable :: boolean))
    (invoke-special hashtable (this) '*init*
                    ht:equivalenceFunction
                    ht:hashFunction
                    (if mutable (+ (ht:size) 100) (ht:size)))
    (putAll ht)
    (set! (this):mutable mutable))
  ((hash key) :: int
   (hashFunction key))
  ((matches value1 value2) :: <boolean>
   (equivalenceFunction value1 value2))
  ((walk (proc :: <procedure>)) :: <void>
   (hash-table-walk% (this) node (proc (*:getKey node) (*:getValue node))))
  ((fold (proc :: <procedure>) acc)
   (hash-table-walk% (this) node
		     (set! acc (proc (*:getKey node) (*:getValue node) acc)))
   acc)
  ((keysVector) :: vector
   (let ((v :: vector (gnu.lists.FVector)))
     (hash-table-walk% (this) node
		       (v:add (node:getKey)))
     v))
  ((entriesVectorPair) :: pair
   (let ((keys :: vector (gnu.lists.FVector))
	 (vals :: vector (gnu.lists.FVector)))
     (hash-table-walk% (this) node
		       (begin (keys:add (node:getKey))
			      (vals:add (node:getValue))))
     (cons keys vals)))
  ((toAlist)
   (let ((result '()))
     (hash-table-walk% (this) node
		       (set! result (cons
				     (cons (*:getKey node) (*:getValue node))
				     result)))
     result))
  ((toNodeList) ::list
   (let ((result '()))
     (hash-table-walk% (this) node
		       (set! result (cons node result)))
     result))
  ((toNodeArray) ::hashnode[]
   (let* ((n ::int ((this):size))
	  (result (hashnode[] length: n))
	  (i ::int 0))
     (hash-table-walk% (this) node
		       (begin (set! (result i) node)
			      (set! i (+ i 1))))
     result))
  ((putAll (other :: hashtable)) :: <void>
   (hash-table-walk% other node
		     (*:put (this) (*:getKey node) (*:getValue node))))
  ((clone) (make hashtable (this) #t))
  )

(define (hashtable-check-mutable (ht :: hashtable)) :: void
  (if (not ht:mutable)
      (error "cannot modify non-mutable hashtable")))

(define-constant min-depth :: int 4)
(define-class TreeNode ()
  (left :: TreeNode)
  (right :: TreeNode)
  (item :: int)
  ((item-check) :: int
   (if (eq? left #!null)
       item
       (+ item (- (left:item-check) (right:item-check))))))

(define (bottom-up-tree (item :: int) (depth :: int)) :: TreeNode
  (if (= depth 0)
      (TreeNode item: item)
      (let ((item2 :: int (* 2 item))
	    (depth-1 :: int (- depth 1)))
	(TreeNode left: (bottom-up-tree (- item2 1) depth-1)
		  right: (bottom-up-tree item2 depth-1)
		  item: item))))

(define (test (n :: int))
  (define max-depth :: int (if (> (+ min-depth 2) n) (+ min-depth 2) n))
  (define stretch-depth (+ max-depth 1))
  (format #t "stretch tree of depth ~d~c check: ~d~%" stretch-depth #\tab
          ((bottom-up-tree 0 stretch-depth):item-check))
  (define long-lived-tree (bottom-up-tree 0 max-depth))
  (do ((d :: int min-depth (+ d 2)))
      ((> d max-depth) #!void)
    (let ((iterations :: int (bitwise-arithmetic-shift-left
			      1 (+ max-depth min-depth (- d))))
	  (check :: int 0))
      (format #t "~d~c trees of depth ~d~c check: ~d~%"
	      (* iterations 2) #\tab d #\tab
	      (do ((i :: int 1 (+ 1 i)))
		  ((> i iterations) check)
		(set! check (+ check
			       ((bottom-up-tree i d):item-check)
			       ((bottom-up-tree (- i) d):item-check)))))))
  (format #t "long lived tree of depth ~d~c check: ~d~%"
	  max-depth #\tab (long-lived-tree:item-check)))

(define N (string->number (cadr (command-line))))
(test N)

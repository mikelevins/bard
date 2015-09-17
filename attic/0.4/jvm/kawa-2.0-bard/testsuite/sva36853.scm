(module-compile-options warn-as-error: #t)

(define (my-member (key String) (strings java.util.List[String]))
  (let ((iter (strings:iterator)))
    (let loop ()
      (if (iter:hasNext)
	  (let ((next (iter:next)))
	    (if (eq? (next:intern) key)
		#t
		(loop)))
	  #f))))

(define strlist ::java.util.List[String]
  ["ab" "cd" "ef"])
(format #t "~s~%" strlist)
;; Output: #("ab" "cd" "ef")
(format #t "~s ~s~%"
        (my-member "cd" strlist)
        (my-member "gg" strlist))
;; Output: #t #f

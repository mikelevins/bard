(module-static 'init-run)

(define-private verbose #f)

;; Your basic bean
(define-simple-class <A> (<Base>)
  (target-axis-name init: "field value")
  ((get-target-axis-name) :: <String>
   (if verbose
       (display (format "    ~A (A.getter() called)~%" target-axis-name)))
   target-axis-name)
  ((set-target-axis-name (v :: <String>)) :: <void>
   (if verbose
       (display (format "    ~A (A.setter() called)~%" v)))
   (set! target-axis-name v)))

;; Bean with "hidden" field
(define-simple-class <A2> (<Base>)
  (hidden-target-axis-name init: "field value")
  ((get-target-axis-name) :: <String>
   (if verbose
       (display (format "    ~A (A2.getter() called)~%"
			hidden-target-axis-name)))
   hidden-target-axis-name)
  ((set-target-axis-name (v :: <String>)) :: <void>
   (if verbose
       (display (format "    ~A (A2.setter() called)~%" v)))
   (set! hidden-target-axis-name v)))

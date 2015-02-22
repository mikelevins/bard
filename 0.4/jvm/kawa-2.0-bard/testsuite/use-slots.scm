(module-static #t)

(test-begin "slot-mangling" 15)

(define slot-name 'target-axis-name)
(define tzoffset (slot-ref (make <java.util.Date>) 'timezone-offset))

(define (run-me)
  (let ((a :: <Base> (make <A>)))
    (test-begin "A through Base")
    (test-equal "invoke getTargetAxisName()"
		"field value" (invoke a 'getTargetAxisName))
    (test-equal "slot-ref with literal" "field value"
		(slot-ref a 'target-axis-name))
    ;; slot-set! with literal
    (slot-set! a 'target-axis-name 'still-from-field)
    ;; slot-ref with runtime symbol
    (test-equal "still-from-field" (slot-ref a slot-name))
    ;; slot-set! with runtime symbol
    (slot-set! a slot-name 'still-still-from-field)
    (test-end))

  (let ((a :: <A> (make <A>)))
    (test-begin "A as A")
    (test-equal "invoke getTargetAxisName()"
		"field value" (invoke a 'getTargetAxisName))
    (test-equal "slot-ref with literal" "field value"
		(slot-ref a 'target-axis-name))
    ;; slot-set! with literal
    (slot-set! a 'target-axis-name 'still-from-field)
    ;; slot-ref with runtime symbol
    (test-equal '|still-from-field| (slot-ref a slot-name))
    ;; slot-set! with runtime symbol
    (slot-set! a slot-name 'still-still-from-field)
    (test-end))
                        
  (let ((a :: <Base> (make <A2>)))
    (test-begin "A2 through Base")
    (test-equal "invoke getTargetAxisName()"
		"field value" (invoke a 'getTargetAxisName))
    (test-equal "slot-ref with literal" "field value"
		(slot-ref a 'target-axis-name))
    ;; slot-set! with literal
    (slot-set! a 'target-axis-name 'still-from-field)
    ;; slot-ref with runtime symbol
    (test-equal "still-from-field" (slot-ref a slot-name))
    ;; slot-set! with runtime symbol
    (slot-set! a slot-name 'still-still-from-field)
    (test-end))

  (let ((a :: <A2> (make <A2>)))
    (test-begin "A2 as A2")
    (test-equal "invoke getTargetAxisName()"
		"field value" (invoke a 'getTargetAxisName))
    (test-equal "slot-ref with literal" "field value"
		(slot-ref a 'target-axis-name))
    ;; slot-set! with literal
    (slot-set! a 'target-axis-name 'still-from-field)
    ;; slot-ref with runtime symbol
    (test-equal "still-from-field" (slot-ref a slot-name))
    ;; slot-set! with runtime symbol
    (slot-set! a slot-name 'still-still-from-field)
    (test-end))
                        
  ;; We assume getTimeZoneOffset is always a multiple of 30.
  (test-equal 0 (modulo tzoffset 30))
  (test-equal 15 (modulo (+ 45 tzoffset) 30)))

(run-me)

;; Based on Savannah bug #39048: Bad method call resolution?
(define (target-axis-name argument)
  (format "from top-level with argument ~a" argument))
(let ((simple
       (object (Base)
               ((setTargetAxisName v::String)
                (error "setTargetAxisName called"))
               ((getTargetAxisName)
                (target-axis-name "from create-simple")))))
  (test-equal "from top-level with argument from create-simple"
              (invoke simple 'getTargetAxisName)))

(test-end)

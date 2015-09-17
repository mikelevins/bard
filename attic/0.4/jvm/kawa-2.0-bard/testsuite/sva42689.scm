;; Savannah bug #42689:
;; Bytecode verify error involving use of "location"

(define-macro (increment! var delta)
    (let ((loc (gentemp)))
      `(let ((,loc (location ,var))) (set! (,loc) (+ (,loc) ,delta))))
)

(define-simple-class accumulator ()
    (value::double 0)
    ;if the next line is uncommented, and the line after that commented, the example works as expected
    ;((accumulate delta::double) (increment! (field (this) "value") delta))
    ((accumulate delta::double) (increment! value delta))
)

(define x (accumulator))
(x:accumulate .5)
(display x:value) (newline)
;; Output: 0.5

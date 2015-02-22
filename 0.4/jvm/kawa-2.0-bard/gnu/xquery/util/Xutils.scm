;; Utility functions primarily for XQuery and XSLT.

(module-compile-options full-tailcalls: #t)

(define-alias BooleanValue gnu.xquery.util.BooleanValue)
(define-alias StringUtils gnu.xquery.util.StringUtils)
(define-alias Math java.lang.Math)

(define (count-values arg) ::int
  (if (instance? arg gnu.mapping.Values)
      ((as gnu.mapping.Values arg):size)
      1))

(define (every proc::procedure values)::boolean
  (every-or-some-values proc values #t))

(define (some proc::procedure values)::boolean
  (every-or-some-values proc values #f))

(define-private (every-or-some-values proc::procedure values match-all::boolean)::boolean
  (if (instance? values gnu.mapping.Values)
      (let* ((v ::gnu.mapping.Values values)
             (ipos ::int 0))
        (let loop ()
          (set! ipos (v:nextPos ipos))
          (if (= ipos 0)
              match-all
              (let* ((item (v:getPosPrevious ipos))
                     (ok (BooleanValue:booleanValue (proc item))))
                (if (eqv? ok match-all)
                    (loop)
                    ok)))))
      (BooleanValue:booleanValue (proc values))))

(define (sublist seq arg1 #!optional (arg2 #!null))
  (let* ((d1 ::double (Math:round (StringUtils:asDouble arg1)))
         (d2 ::double (if (eq? arg2 #!null) java.lang.Double:POSITIVE_INFINITY
                          (+ d1 (Math:round (StringUtils:asDouble arg2))))))
    (gnu.xquery.util.SequenceUtils:subList seq d1 d2)))

(define (integerRange first last)
  (gnu.xquery.util.IntegerRange:integerRange first last))

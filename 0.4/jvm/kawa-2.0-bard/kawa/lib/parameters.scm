;; This implements SRFI-39 "Parameter objects".
(module-export make-parameter as-location%)

(require <kawa.lib.prim_syntax>)

(define (make-parameter init #!optional (converter #!null))
  (if (not (eq? converter #!null))
      (set! init (converter init)))
  (let ((loc (gnu.mapping.ThreadLocation:new)))
    (invoke loc 'setGlobal init)
    (gnu.mapping.LocationProc:new loc converter)))

(define (as-location% param) :: <gnu.mapping.Location>
  (if (instance? param <gnu.mapping.LocationProc>)
      (gnu.mapping.LocationProc:getLocation param)
      (as <gnu.mapping.Location> param)))


(in-package :bard)

(defparameter $env1 
  (extend-env (null-env)
              'x 1
              'y 22
              'z 333))

(compile '(|abort| z) $env1)

(compile '(|and|) $env1)
(compile '(|and| x) $env1)
(compile '(|and| x y) $env1)
(compile '(|and| x y z) $env1)

(compile '(|begin|) $env1)
(compile '(|begin| x) $env1)
(compile '(|begin| x y) $env1)
(compile '(|begin| |x| |y| |z|) $env1)

(compile
 '(|case| x
   (1 'one)
   ((2 3) 'several)
   (:else 'who-knows?))
 $env1)

(compile 
 '(|cond|
   ((friday?) (celebrate 'good-times))
   ((saturday?) (party :hearty))
   ((sunday?) (cherish (cherry-blossoms)))
   (:else "Whatever"))
 $env1)

(compile '(|def| z 333) $env1)

(compile '(|define| |class| |Point| ()) $env1)

(compile '(|define| |condition| |WonkyLooking| (|JDLR|) (a b c)) $env1)

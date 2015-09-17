(import (rename (lib2 "aux-libs.scm") (twice times2)))
(import (lib3 "aux-libs.scm"))

(format #t "(times2 7)=~w; (thrice 7)=~w~%" (times2 7) (thrice 7))
;; Output: (times2 7)=14; (thrice 7)=21

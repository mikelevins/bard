(test-begin "srfi-108")

;; Some tests are based on Racket/Scribble documentation.

(require "test-utils.scm")

(define-syntax $construct$:doc
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<div>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ div) arg ...))))

(define-syntax $construct$:chapter
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<h1>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ h1) arg ...))))

(define-syntax $construct$:section
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<h2>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ h2) arg ...))))

(define-syntax $construct$:itemlist
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<ul>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ ul) arg ...))))

(define-syntax $construct$:item
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<li>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ li) arg ...))))

(define-syntax $construct$:emph
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<em>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ em) arg ...))))

(xtest &emph{abc}
       '($construct$:emph "abc")
       &{<em>abc</em>})

(xtest &emph{abc&["n1" 3 "m2"]z}
       '($construct$:emph "abc" $<<$ "n1" 3 "m2" $>>$ "z")
       &{<em>abcn13m2z</em>})

(xtest &emph{abc&(+ 3 4)z}
       '($construct$:emph "abc" $<<$ (+ 3 4) $>>$ "z")
       &{<em>abc7z</em>})

(xtest &itemlist[&item{We have cookies for you.}
                 &item{If you want to eat a cookie,
                       you must bring your own straw.}]
       '($construct$:itemlist
         ($construct$:item "We have cookies for you.")
         ($construct$:item "If you want to eat a cookie,
                       you must bring your own straw.")
         $>>$)
       "<ul><li>We have cookies for you.</li><li>If you want to eat a cookie,\n                       you must bring your own straw.</li></ul>")

(define (make-foo . args) (apply list 'FOO args))

(define-simple-constructor foo make-foo)

(test-equal '(FOO "abc7z")
            &foo{abc&(+ 3 4)z})
(test-equal '(FOO 3 20 "abc7z")
            &foo[(+ 1 2) (* 4 5)]{abc&(+ 3 4)z})
(test-equal '(FOO id: "n7" "7abc")
            &foo[id: "n7"]{&(+ 3 4)abc})

(define-syntax $construct$:bar
  (syntax-rules ()
    ((_ . args) (list 'BAR . args))))
(test-equal '(BAR "abc" "" 7 "" "z")
            &bar{abc&(+ 3 4)z})
(test-equal '(BAR 3 20 "" "abc" "" 7 "" "z")
            &bar[(+ 1 2) (* 4 5)]{abc&(+ 3 4)z})

(test-end)

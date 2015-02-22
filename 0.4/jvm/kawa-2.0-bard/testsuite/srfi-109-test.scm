(test-begin "srfi-109")

(cond-expand
 (kawa
  ;; Using 3-operand datum->syntax enables line numbers in reporting.
  (define-syntax strtest
    (lambda (form)
      (syntax-case form ()
        ;; We need to use the rest1 and rest2 variables since the Kawa reader
        ;; currently only attaches line-numbers to pairs, and the quoted and
        ;; evaluated sub-forms aren't guaranteed to be lists.
        ((strtest value . rest1)
         (syntax-case #'rest1 ()
           ((quoted . rest2)
            (syntax-case #'rest2 ()
              ((evaluated)
               #`(begin
                   #,(datum->syntax form #'(test-equal quoted (quote value))
                                    #'rest1)
                   #,(datum->syntax form #'(test-equal evaluated value)
                                    #'rest2)))))))))))
 (else
  (define-syntax strtest
    (syntax-rules ()
      ((strtest string quoted evaluated)
       (begin
         (test-equal quoted (quote string))
         (test-equal evaluated string)))))))

(strtest &{abc}
         '($string$ "abc")
         "abc")
(strtest &{ab&(+ 3 4)xz}
         '($string$ "ab" $<<$ (+ 3 4) $>>$ "xz")
         "ab7xz")
(strtest &{ab&[(+ 3 4)]xz}
         '($string$ "ab" $<<$ (+ 3 4) $>>$ "xz")
         "ab7xz")
;; Literal nested braces.
(strtest &{ab{x}{}c{{d}}}
         '($string$ "ab{x}{}c{{d}}")
         "ab{x}{}c{{d}}")
;; Literal nested braces with enclosed expression.
(strtest &{ab{&[(+ 5 7)]c}z}
         '($string$ "ab{" $<<$ (+ 5 7) $>>$ "c}z")
         "ab{12c}z")
(strtest &{ab&[3 4]xzy}
         '($string$ "ab" $<<$ 3 4 $>>$ "xzy")
         "ab34xzy")
(strtest &{_&lbrace;_&rbrace;_&gt;_&lt;_&quot;_&apos;_}
         '($string$ "_" $entity$:lbrace "_" $entity$:rbrace "_" $entity$:gt
                    "_" $entity$:lt "_" $entity$:quot "_" $entity$:apos "_")
         "_{_}_>_<_\"_'_")

(strtest &{_&alarm;_&backspace;_&delete;_&escape;_&newline;_&null;_&return;_&space;_&tab;_}
         '($string$ "_" $entity$:alarm "_" $entity$:backspace
                    "_" $entity$:delete "_" $entity$:escape "_" $entity$:newline
                    "_" $entity$:null "_" $entity$:return "_" $entity$:space
                    "_" $entity$:tab "_")
         "_\a_\b_\x7f;_\x1b;_\n_\x0;_\r_ _\t_")

(strtest &{a
b}
         '($string$ "a\nb")
         "a\nb")

(strtest &{_&#64;_&#x3f;_&#125;_}
         '($string$ "_@_?_}_")
         "_@_?_}_")

(strtest &{abc&#|comment|#xyz} '($string$ "abcxyz") "abcxyz")

(strtest &{abc
    &|def
    &| klm}
         '($string$ "abc\ndef\n klm")
         "abc\ndef\n klm")

(strtest &{
    &|def
    &| klm}
         '($string$ "def\n klm")
         "def\n klm")

;; Next line is supposed to have trailing whitespace - should be ignored.
(strtest &{  
    &|def
    &| klm}
         '($string$ "def\n klm")
         "def\n klm")

(test-equal
 "\n  ab\n  cd\n"
 (test-read-eval-string "&{\n  ab\n  cd\n}"))

(test-equal
 " ab\n cd\n"
 (test-read-eval-string "&{\n &| ab\n &| cd\n}"))

(test-equal
 "\n\n ab\n cd\n"
 (test-read-eval-string "&{\n\n &| ab\n &| cd\n}"))

(test-equal
 "\n ab\n cd\n"
 (test-read-eval-string "&{&#||#\n &| ab\n &| cd\n}"))

(test-equal
 "\n ab\n cd\n"
 (test-read-eval-string "&{&[]\n &| ab\n &| cd\n}"))

(test-equal
 " ab\n cd\n"
 (test-read-eval-string "&{   \n &| ab\n &| cd\n}"))

(test-equal
 "line1\nline2\n"
 (test-read-eval-string "&{
     &|line1
     &|line2
     &|}"))

(test-equal
 "line1\nline2\n"
 (test-read-eval-string "&{\n     &|line1\n     &|line2\n}"))

(test-equal
 " k \n ab\n cd\n"
 (test-read-eval-string "&{ k \n &| ab\n &| cd\n}"))

(test-equal
 "   \n ab\n cd\n"
 (test-read-eval-string "&{ &space; \n &| ab\n &| cd\n}"))

(strtest &{&space;
    &|def
    &| klm}
         '($string$ $entity$:space "\ndef\n klm")
         " \ndef\n klm")

(strtest &{abc&-
  def&-
  &| klm}
         '($string$ "abc  def klm")
         "abc  def klm")

(strtest &{<&[(string-length "a/b/c")]>}
         '($string$ "<" $<<$ (string-length "a/b/c")  $>>$ ">")
         "<5>")

(strtest &{m&[3]&[4]n}
         '($string$ "m" $<<$ 3 $>>$ $<<$ 4 $>>$ "n")
         "m34n")

;; Some tests using format
(strtest &{abc&~3d(+ 4 5)z}
         '($string$ "abc" ($format$ "~3d" (+ 4 5)) "z")
         "abc  9z")

(strtest &{A&~{[]<&[[5 6 7]]>&~}[]Z}
         '($string$ "A" ($format$ "~{") "<" $<<$ ($bracket-list$ 5 6 7)
                    $>>$ ">" ($format$ "~}") "Z")
         "A<5><6><7>Z")

;; Same as above, but with ellided empty []
(strtest &{A&~{<&[[5 6 7]]>&~}Z}
         '($string$ "A" ($format$ "~{") "<" $<<$ ($bracket-list$ 5 6 7)
                    $>>$ ">" ($format$ "~}") "Z")
         "A<5><6><7>Z")

(strtest &{[&~{&[[5 6 7]]&~^_&~}]}
         '($string$ "[" ($format$ "~{") $<<$ ($bracket-list$ 5 6 7)
                    $>>$ ($format$ "~^") "_" ($format$ "~}") "]")
         "[5_6_7]")

(strtest &{[&~{&[[]]&~^_&~}]}
         '($string$ "[" ($format$ "~{") $<<$ ($bracket-list$)
                    $>>$ ($format$ "~^") "_" ($format$ "~}") "]")
         "[]")

(strtest &{_&~4t~w["qwerty"]_}
         '($string$ "_" ($format$ "~4t~w" "qwerty") "_")
         &{_   "qwerty"_})

(cond-expand (kawa
              (strtest &{X&[@(list 3 4)]Y}
                       '($string$ "X" $<<$ ($splice$ (list 3 4)) $>>$ "Y")
                       "X34Y")
              (strtest &{X&~w[@(list "x" "y")]Y}
                       '($string$ "X" ($format$ "~w" ($splice$ (list "x" "y")))
                                  "Y")
                       &{X"x"Y})
              (strtest &{X&~w[@(list "x" "y")]&~w[]Y}
                       '($string$ "X" ($format$ "~w" ($splice$ (list "x" "y")))
                                  ($format$ "~w") "Y")
                       &{X"x""y"Y})
              ))

(test-end)

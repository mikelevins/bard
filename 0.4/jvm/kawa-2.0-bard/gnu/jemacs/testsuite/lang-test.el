;(load "/home/bothner/kawa-bin/testsuite/testing.zip")
(test-init "elisp language" 24)

(setq y 100)
(defun foo1 (x)
  (+ x y))
(defun foo2 (y)
  (foo1 30))
(test 50 'fluid-test-1 (foo2 20))

;(test t 'eq-test-1 (eq t 't))

(test t 'equal-test-1 (equal "tt" "tt"))
(test nil 'equal-test-1 (equal "tt" "tt "))

(test "The octal value of 18 is 22,
   and the hex value is 12."
      'format-test-1
      (format "The octal value of %d is %o,
   and the hex value is %x." 18 18 18))

(test "[000123]" 'format-test-2 (format "[%06d]" 123))

(defun integer-format-test-2 (result fmt i1 i2)
  (test result 'integer-format-test (format (concat fmt fmt) i1 i2)))
;; Tests from Harbison & Steele "C - A Reference Manual" (3rd edition):
;; Test %d:
(integer-format-test-2 "[          45][         -45]" "[%12d]" 45 -45)
(integer-format-test-2 "[000000000045][-00000000045]" "[%012d]" 45 -45)
(integer-format-test-2 "[ 00000000045][-00000000045]" "[% 012d]" 45 -45)
(integer-format-test-2 "[         +45][         -45]" "[%+12d]" 45 -45)
(integer-format-test-2 "[+00000000045][-00000000045]" "[%+012d]" 45 -45)
(integer-format-test-2 "[45          ][-45         ]" "[%-12d]" 45 -45)
(integer-format-test-2 "[ 45         ][-45         ]" "[%- 12d]" 45 -45)
(integer-format-test-2 "[+45         ][-45         ]" "[%-+12d]" 45 -45)
(integer-format-test-2 "[        0045][       -0045]" "[%12.4d]" 45 -45)
(integer-format-test-2 "[0045        ][-0045       ]" "[%-12.4d]" 45 -45)
;; Test %x:
(setq neg45 (logand -45 #xffffffff))
(integer-format-test-2 "[          2d][    ffffffd3]" "[%12x]" 45 neg45)
(integer-format-test-2 "[00000000002d][0000ffffffd3]" "[%012x]" 45 neg45)
(integer-format-test-2 "[        0X2D][  0XFFFFFFD3]"  "[%#12X]" 45 neg45)
(integer-format-test-2 "[0X000000002D][0X00FFFFFFD3]" "[%#012X]" 45 neg45)
(integer-format-test-2 "[2d          ][ffffffd3    ]" "[%-12x]" 45 neg45)
(integer-format-test-2 "[0x2d        ][0xffffffd3  ]" "[%-#12x]" 45 neg45)
(integer-format-test-2 "[        002d][    ffffffd3]" "[%12.4x]" 45 neg45)
;; Harbison&Steele (4th ed) have a typo here.
(integer-format-test-2 "[0x002d      ][0xffffffd3  ]" "[%-#12.4x]" 45 neg45)

(setq i 0)
(setq j 1)
(while (< i 6)
  (setq i (+ i 1))
  (setq j (* j i)))
(test 720 'while-test-1 j)

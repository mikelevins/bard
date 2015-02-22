(test-begin "system")

(define split-with-kawa
  ["../bin/kawa.sh" "-e" "(format #t \"|~{~a|~}~%\" command-line-arguments)"])

(define-syntax test-str-equal
  (syntax-rules ()
    ((_ expected value)
     (test-equal expected (->string value)))))

(define p1 &`{echo foo bar})
(test-equal "[foo bar\n]" (format #f "[~a]" p1))

(define p2 &`[in: &`{echo bar bar baz}]{tr a-m A-M})
(test-equal "[BAr BAr BAz\n]" (format #f "[~a]" p2))

(define p3 (run-process "echo foo baz"))
(test-equal "[foo baz\n]" (format #f "[~a]" p3))

;; Run this manually in the REPL (perhaps with a longer sleep time)
;; to verify incremental output.
(define p4 &`{sh -c 'for x in $*; do echo $x; sleep 0.1; done; echo Done' "-" "α" "β" "γ"})
(test-equal "[α\nβ\nγ\nDone\n]" (format #f "[~a]" p4))

(test-str-equal "|abc|def ghi|\n" &`{&[split-with-kawa] abc "def ghi"})

(let ((v1 " x yz"))
  (test-str-equal "|x|yzabc|x|yz|def x yzghi|\n"
                  &`{&[split-with-kawa] &[v1]abc &[v1] "def&[v1]ghi"}))

(let ((v1 " x yz"))
  (test-str-equal "|x|yzabc|x|yz|def x yzghi|\n"
                  &sh{&[split-with-kawa] &[v1]abc &[v1] "def&[v1]ghi"}))

(let ((v2 ["a'b" "c\"d"]))
  (test-str-equal "|cmd|a'b|c\"d|\n" &`{&[split-with-kawa] cmd &[v2]}))

(let ((v2 ["a'b" "c\"d"]))
  (test-str-equal "|cmd|a'b|c\"d|\n" &sh{&[split-with-kawa] cmd &[v2]}))

(let ((v2 ["a b" "c\"d"]))
  (test-str-equal "|cmd|a b c\"d|\n" &`{&[split-with-kawa] cmd  "&[v2]"}))

(let ((vq (unescaped-data "' bc '")))
  (test-str-equal "|cmd|a|bc|z|\n"
                  &`{&[split-with-kawa] cmd 'a&[vq]z'}))
(let ((vq (unescaped-data "' bc '")))
  (test-str-equal "|shcmd|a|bc|z|\n"
                  &sh{&[split-with-kawa] shcmd 'a&[vq]z'}))
;; In contrast with:
(let ((vq "' bc '"))
  (test-str-equal "|cmd|a' bc 'z|\n"
                  &`{&[split-with-kawa] cmd 'a&[vq]z'}))
(let ((vu (unescaped-data "b ' c d '")))
  (test-str-equal "|cmd|a b |c|d|z|\n"
                  &`{&[split-with-kawa] cmd 'a &[vu]z'}))

;; Check final-newline-removal
(test-str-equal "|xa/by|\n"
                 &`{&[split-with-kawa] x&["a/b"]y})
(test-str-equal "|xa/by|\n"
                 &`{&[split-with-kawa] x&["a/b\n"]y})
(test-str-equal "|xa/by|\n"
                 &`{&[split-with-kawa] x&["a/b\n\n"]y})
(test-str-equal "|xa|cy|\n"
                 &`{&[split-with-kawa] x&["a\nc"]y})
(test-str-equal "|xa|cy|\n"
                 &`{&[split-with-kawa] x&["a\n\nc"]y})
(test-str-equal "|xa|cy|\n"
                 &`{&[split-with-kawa] x&["a\n\nc\n"]y})
(test-str-equal "|xa|cy|\n"
                 &`{&[split-with-kawa] "x&["a\nc"]y"})
(test-str-equal "|xa\ncy|\n"
                 &`{&[split-with-kawa] 'x&["a\nc"]y'})
(test-str-equal "|xa||cy|\n"
                 &`{&[split-with-kawa] "x&["a\n\nc"]y"})
(test-str-equal "|xa\n\ncy|\n"
                 &`{&[split-with-kawa] 'x&["a\n\nc"]y'})
(test-str-equal "|xa||cy|\n"
                 &`{&[split-with-kawa] "x&["a\n\nc\n"]y"})
(test-str-equal "|xa\n\ncy|\n"
                 &`{&[split-with-kawa] 'x&["a\n\nc\n"]y'})
(test-str-equal "|xa/by|\n"
                 &sh{&[split-with-kawa] x&["a/b"]y})
(test-str-equal "|xa/by|\n"
                 &sh{&[split-with-kawa] x&["a/b\n"]y})
(test-str-equal "|xa/by|\n"
                 &sh{&[split-with-kawa] x&["a/b\n\n"]y})
(test-str-equal "|xa|cy|\n"
                 &sh{&[split-with-kawa] x&["a\nc"]y})
(test-str-equal "|xa|cy|\n"
                 &sh{&[split-with-kawa] x&["a\n\nc"]y})
(test-str-equal "|xa|cy|\n"
                 &sh{&[split-with-kawa] x&["a\n\nc\n"]y})
(test-str-equal "|xa|cy|\n"
                 &sh{&[split-with-kawa] "x&["a\nc"]y"})
(test-str-equal "|xa\ncy|\n"
                 &sh{&[split-with-kawa] 'x&["a\nc"]y'})
(test-str-equal "|xa||cy|\n"
                 &sh{&[split-with-kawa] "x&["a\n\nc"]y"})
(test-str-equal "|xa\n\ncy|\n"
                 &sh{&[split-with-kawa] 'x&["a\n\nc"]y'})
(test-str-equal "|xa||cy|\n"
                 &sh{&[split-with-kawa] "x&["a\n\nc\n"]y"})
(test-str-equal "|xa\n\ncy|\n"
                 &sh{&[split-with-kawa] 'x&["a\n\nc\n"]y'})

(let ((tmp1 (java.io.File:createTempFile "kawa-test" #!null)))
  &`[out-to: tmp1]{echo ab cd}
  &`[out-to: tmp1]{echo cd ef}
  &`[out-append-to: tmp1]{echo gh ij}
  (test-equal "cd ef\ngh ij\n" (utf8->string (path-bytes tmp1)))
  (tmp1:delete))

(let ((strport (open-output-string)))
  (write-string "(* " strport)
  &`[out-to: strport]{echo -n foo bar}
  (sleep 0.1) ;; This is rather clunky.
  (write-string " *)" strport)
  (test-equal "(* foo bar *)" (get-output-string strport)))

(parameterize
 ((current-output-port (open-output-string)))
 (write-string "(* ")
 &`[out-to: 'current]{echo -n bar baz}
 (sleep 0.1) ;; This is rather clunky.
 (write-string " *)")
 (test-equal "(* bar baz *)" (get-output-string (current-output-port))))

(let ((strport (open-output-string)))
  (write-string "(* " strport)
  &`[err-to: strport]{grep foo file-does-not-exist}
  (sleep 0.1) ;; This is rather clunky.
  (write-string " *)" strport)
  (test-equal "(* grep: file-does-not-exist: No such file or directory\n *)"
              (get-output-string strport)))

(parameterize ((current-input-port (open-input-bytevector (string->utf8 "Hællø World!\n"))))
              (test-str-equal "HÆLLø WorLD!\n" &`[in-from: 'current]{tr a-mæ A-MÆ}))

(parameterize ((current-input-port (open-input-string "Hællø World!\n")))
              (test-str-equal  "HÆLLø WorLD!\n" &`[in-from: 'current]{tr a-mæ A-MÆ}))

(let ((strport (open-input-string "Hello World!\n")))
  (test-str-equal "HELLo WorLD!\n" &`[in-from: strport]{tr a-m A-M}))

(test-equal 0 (process-exit-wait (run-process "echo foo")))
(test-equal #t (process-exit-ok? (run-process "echo foo")))
(test-equal #f (process-exit-ok? (run-process "/bin/false")))
(test-equal #t (process-exit-ok? (run-process "/bin/true")))

(test-end)

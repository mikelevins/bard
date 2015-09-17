(module-name (kawa string-cursors))

(module-export string-cursor-start string-cursor-end
               string-cursor-ref substring-cursor
               string-cursor-next string-cursor-prev
               string-cursor<? string-cursor<=? string-cursor=?
               string-cursor>? string-cursor>=? string-cursor
               string-cursor-for-each)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(import (rename (only (gnu kawa lispexpr LangPrimType) stringCursorType)
                (stringCursorType string-cursor)))

;; returns a cursor for the start of the string
(define (string-cursor-start str::string) ::string-cursor
  (as string-cursor 0))

;; returns a cursor one past the last valid cursor
(define (string-cursor-end str::string) ::string-cursor
  (as string-cursor (str:length)))

;; get the char at the given cursor
(define (string-cursor-ref str::string cursor::string-cursor) ::character
  (as character (java.lang.Character:codePointAt str (as int cursor))))

#|
(define (string-cursor-ref-or-eof str::string cursor::string-cursor) ::character-or-eof
  (let ((len (str:length))
        (cursor0 (as int cursor)))
    (if (>= cursor0 len) #!eof
        (let ((ch1 ::int (str:charAt cursor0)))
          (if (or (< ch1 #xD800) (> ch1 #xDBFF))
              (as character-or-eof ch1)
              (let ((cursor1 (+ cursor0 1)))
                (if (>= cursor1 len) #!eof
                    (let ((ch2 ::int (str:charAt cursor1)))
                      (if (and (>= ch2 #xDC00) (<= ch2 #xDFFF))
                          (as character-or-eof 
                              (+ (* (- ch1 #xD800) #x400)
                                 (- ch2 #xDC00)
                                 #x10000))
                          (as character-or-eof ch1))))))))))
|#

;; increment to the next cursor
(define (string-cursor-next str::string cursor::string-cursor
                            #!optional (count ::int 1)) ::string-cursor
  (as string-cursor
      (java.lang.Character:offsetByCodePoints str (as int cursor) count)))

;; decrement to the previous cursor
(define (string-cursor-prev str::string cursor::string-cursor
                            #!optional (count ::int 1)) ::string-cursor
  (as string-cursor
      (java.lang.Character:offsetByCodePoints str (as int cursor) (- count))))

;; take a substring from the given cursors
(define (substring-cursor str::string cs1::string-cursor
                          #!optional (cs2::string-cursor (as string-cursor (str:length)))) ::string
  (str:subSequence (as int cs1) (as int cs2)))

;; cs1 is before cs2
(define (string-cursor<? cs1::string-cursor cs2::string-cursor) ::boolean
  validate-apply: "kawa.lib.compile_misc:stringCursorCompareValidateApply"
  (< (as int cs1) (as int cs2)))

;; cs1 is before or the same as cs2
(define (string-cursor<=? cs1::string-cursor cs2::string-cursor) ::boolean
  validate-apply: "kawa.lib.compile_misc:stringCursorCompareValidateApply"
  (<= (as int cs1) (as int cs2)))

;; cs1 is the same as cs2
(define (string-cursor=? cs1::string-cursor cs2::string-cursor) ::boolean
  validate-apply: "kawa.lib.compile_misc:stringCursorCompareValidateApply"
  (= (as int cs1) (as int cs2)))

;; cs1 is after cs2
(define (string-cursor>? cs1::string-cursor cs2::string-cursor) ::boolean
  validate-apply: "kawa.lib.compile_misc:stringCursorCompareValidateApply"
  (> (as int cs1) (as int cs2)))

;; cs1 is the same or after cs2
(define (string-cursor>=? cs1::string-cursor cs2::string-cursor) ::boolean
  validate-apply: "kawa.lib.compile_misc:stringCursorCompareValidateApply"
  (>= (as int cs1) (as int cs2)))

(define (string-cursor-for-each proc str::string
                                #!optional
                                (start::string-cursor (as string-cursor 0))
                                (end::string-cursor (string-cursor-end str)))
  ::void
  validate-apply: "kawa.lib.compile_map:stringCursorForEachValidateApply"
  (do ((cursor::string-cursor start
                              (string-cursor-next str cursor)))
      ((string-cursor>=? cursor end))
    (proc (string-cursor-ref str cursor))))

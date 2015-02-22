(define x1 &{ab&|z})
;; Diagnostic: bad-srfi-109.scm:1:16: invalid '&|'

(define x2 &{a
  bc&|z})
;; Diagnostic: bad-srfi-109.scm:5:3: non-whitespace before '&|'

(define x3 &{a&)z}) ;; Poor error message - not expecting '{' here.  FIXME.
;; Diagnostic: bad-srfi-109.scm:8:16: expected '[', '{', or ';'

(define x5  &{ab&- x
    yx})
;; Diagnostic: bad-srfi-109.scm:11:20: non-whitespace after '&-'

;; The following are emitted later, during name-looking,
;; so to avoid ordering problems they should be later in this file.

(define y1 &{a&klmnqz;z})
;; Diagnostic: bad-srfi-109.scm:18:15: unknown entity name klmnqz


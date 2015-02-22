;; Savannah bug#43341: Import should warn about non existing bindings
(import (only (kawa regex) regexp-match))
;; Diagnostic: errors5.scm:2:28: unknown symbol in import set: regexp-match

(import (except (kawa regex) regex-replace regexp-match))
;; Diagnostic: errors5.scm:5:44: unknown symbol in import set: regexp-match

(import (rename (except (kawa regex) regex-replace)
                (regexp-split regex-split)))
;; Diagnostic: errors5.scm:9:17: missing binding regexp-split

(import (rename (kawa regex) (regex-split regex-replace)))
;; Diagnostic: errors5.scm:12:9: duplicate binding for regex-replace

;;; ---------------------------------------------------------------------
;;; checking for errors
;;; ---------------------------------------------------------------------

(c-define (c:bard-error?) () 
          bool "bard_error_status" ""
          (and $bard-error #t))

(c-define (c:bard-error) () 
          char-string "bard_error" ""
          $bard-error)

(c-define (c:clear-bard-error) () 
          void "clear_bard_error" ""
          (set! $bard-error #f))

;;; ---------------------------------------------------------------------
;;; bard startup
;;; ---------------------------------------------------------------------

(c-define (c:version) () 
          char-string "bard_version" ""
          (cbard:version))

(c-define (c:init-bard) () 
          bool "init_bard" ""
          (cbard:init-bard))

;;; ---------------------------------------------------------------------
;;; bard types
;;; ---------------------------------------------------------------------

(c-define (c:bard-type obj) (scheme-object) 
          int "bard_type" ""
          (cbard:type-for-C obj))

(c-define (c:bard-typename obj) (scheme-object) 
          char-string "bard_typename" ""
          (cbard:typename obj))

;;; ---------------------------------------------------------------------
;;; read-eval-print
;;; ---------------------------------------------------------------------

(c-define (c:bard-read str) (char-string) 
          int "bard_read" ""
          (cbard:read str))

(c-define (c:bard-eval obj) (scheme-object) 
          scheme-object "bard_eval" ""
          (cbard:eval obj))

(c-define (c:bard-print obj) (scheme-object) 
          scheme-object "bard_print" ""
          (cbard:print obj))

;;; ---------------------------------------------------------------------
;;; simple value conversions
;;; ---------------------------------------------------------------------

(c-define (c:as-char obj) (scheme-object) 
          char "as_char" ""
          (cbard:as-char obj))

(c-define (c:as-bool obj) (scheme-object) 
          bool "as_bool" ""
          (cbard:as-bool obj))

(c-define (c:as-int obj) (scheme-object) 
          int "as_int" ""
          (cbard:as-int obj))

(c-define (c:as-float obj) (scheme-object) 
          char "as_float" ""
          (cbard:as-float obj))

(c-define (c:as-string obj) (scheme-object) 
          char-string "as_string" ""
          (cbard:as-string obj))

;;; ---------------------------------------------------------------------
;;; operations on symbols and keywords
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; operations on strings
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; operations on lists
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; operations on frames
;;; ---------------------------------------------------------------------



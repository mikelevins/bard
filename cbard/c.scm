;;; ---------------------------------------------------------------------
;;; checking for errors
;;; ---------------------------------------------------------------------

(c-define (c:bard-error?) () 
          bool "bard_error_status" ""
          (not (null? $bard-errors)))

(c-define (c:bard-last-error-code) () 
          int "bard_last_error_code" ""
          (if (null? $bard-errors)
              $ERR:NO-ERROR
              (error-report-error-code (car $bard-errors))))

(c-define (c:bard-last-error-message) () 
          char-string "bard_last_error_message" ""
          (if (null? $bard-errors)
              "No error"
              (error-report-error-message (car $bard-errors))))

(c-define (c:clear-last-bard-error) () 
          void "clear_last_bard_error" ""
          (clear-last-error-report!))

(c-define (c:clear-all-bard-errors) () 
          void "clear_all_bard_errors" ""
          (clear-error-reports!))

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
          scheme-object "bard_read" ""
          (cbard:read str))

(c-define (c:bard-read-lines str) (char-string) 
          scheme-object "bard_read_lines" ""
          (cbard:read-lines str))

(c-define (c:bard-read-nonempty-lines str) (char-string) 
          scheme-object "bard_read_nonempty_lines" ""
          (cbard:read-nonempty-lines str))

(c-define (c:bard-eval obj) (scheme-object) 
          scheme-object "bard_eval" ""
          (cbard:eval obj))

(c-define (c:bard-load-from-string str) (char-string) 
          scheme-object "bard_load_from_string" ""
          (cbard:load-from-string str))

(c-define (c:bard-print obj) (scheme-object) 
          scheme-object "bard_print" ""
          (cbard:print obj))

;;; ---------------------------------------------------------------------
;;; operations on values
;;; ---------------------------------------------------------------------

(c-define (c:is-empty obj) (scheme-object) 
          bool "bard_is_empty" ""
          (cbard:is-empty? obj))


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



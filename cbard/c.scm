(c-declare
#<<c-code

struct BoxedBardValue {
  int type;
  void* value;
};
typedef struct BoxedBardValue BoxedBardValue;

typedef signed char bool;

BoxedBardValue* bard_box_undefined();
BoxedBardValue* bard_box_null();
BoxedBardValue* bard_box_character(char ch);
BoxedBardValue* bard_box_boolean(bool b);
BoxedBardValue* bard_box_integer(int val);
BoxedBardValue* bard_box_float(float val);
BoxedBardValue* bard_box_ratio(int num, int denom);
BoxedBardValue* bard_box_symbol(const char* str);
BoxedBardValue* bard_box_keyword(const char* str);
BoxedBardValue* bard_box_text(const char* str);

c-code
)


;;; ---------------------------------------------------------------------
;;; memory management
;;; ---------------------------------------------------------------------

(define (%retainable? x)
  (or (pair? x)
      (##subtyped? x)))

(define (%retain obj)
  (if (%retainable? obj)
      (##still-obj-refcount-inc! (##still-copy obj))
      obj))

(c-define (c:bard-retain obj) (scheme-object) 
          scheme-object "bard_retain" ""
          (%retain obj))

(define (%release obj)
  (if (%retainable? obj)
      (##still-obj-refcount-dec! obj)
      obj))

(c-define (c:bard-release obj) (scheme-object) 
          scheme-object "bard_release" ""
          (%release obj))

;;; ---------------------------------------------------------------------
;;; errors
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
;;; initialization
;;; ---------------------------------------------------------------------

(c-define (c:version) () 
          char-string "bard_version" ""
          (cbard:version))

(c-define (c:init-bard) () 
          bool "init_bard" ""
          (cbard:init-bard))

;;; ---------------------------------------------------------------------
;;; types and conversions
;;; ---------------------------------------------------------------------

(c-define (c:bard-type obj) (scheme-object) 
          int "bard_type" ""
          (cbard:type-for-C obj))

(c-define (c:bard-typename obj) (scheme-object) 
          char-string "bard_typename" ""
          (cbard:typename obj))

(c-define (c:is-nothing obj) (scheme-object) 
          bool "bard_is_nothing" ""
          (%nothing? obj))

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
          float "as_float" ""
          (cbard:as-float obj))

(c-define (c:as-string obj) (scheme-object) 
          char-string "as_string" ""
          (cbard:as-string obj))

(define boxed-undefined
  (c-lambda () (pointer "BoxedBardValue")
#<<c-code
  BoxedBardValue* box=bard_box_undefined();
  ___result_voidstar=box;
c-code
))

(define boxed-null
  (c-lambda () (pointer "BoxedBardValue")
#<<c-code
  BoxedBardValue* box=bard_box_null();
  ___result_voidstar=box;
c-code
))

(define as-boxed-character
  (c-lambda (char) (pointer "BoxedBardValue")
#<<c-code
  char ch = ___arg1;
  BoxedBardValue* box=bard_box_character(ch);
  ___result_voidstar=box;
c-code
))

(define as-boxed-boolean 
  (c-lambda (bool) (pointer "BoxedBardValue")
#<<c-code
  bool b = ___arg1;
  BoxedBardValue* box=bard_box_boolean(b);
  ___result_voidstar=box;
c-code
))

(define as-boxed-integer 
  (c-lambda (int) (pointer "BoxedBardValue")
#<<c-code
  int i = ___arg1;
  BoxedBardValue* box=bard_box_integer(i);
  ___result_voidstar=box;
c-code
))

(define as-boxed-float 
  (c-lambda (float) (pointer "BoxedBardValue")
#<<c-code
  float f = ___arg1;
  BoxedBardValue* box=bard_box_float(f);
  ___result_voidstar=box;
c-code
))

(define as-boxed-ratio 
  (c-lambda (int int) (pointer "BoxedBardValue")
#<<c-code
  bool numerator = ___arg1;
  bool denominator = ___arg2;
  BoxedBardValue* box=bard_box_ratio(numerator,denominator);
  ___result_voidstar=box;
c-code
))

(define as-boxed-symbol
  (c-lambda (char-string) (pointer "BoxedBardValue")
#<<c-code
  char* tx = ___arg1;
  BoxedBardValue* box=bard_box_symbol(tx);
  ___result_voidstar=box;
c-code
))

(define as-boxed-keyword
  (c-lambda (char-string) (pointer "BoxedBardValue")
#<<c-code
  char* tx = ___arg1;
  BoxedBardValue* box=bard_box_keyword(tx);
  ___result_voidstar=box;
c-code
))

(define as-boxed-text
  (c-lambda (char-string) (pointer "BoxedBardValue")
#<<c-code
  char* tx = ___arg1;
  BoxedBardValue* box=bard_box_text(tx);
  ___result_voidstar=box;
c-code
))

(c-define (c:as-boxed obj) (scheme-object) 
          (pointer "BoxedBardValue") "as_boxed" ""
          (cond
           ((eq? obj #!unbound)(boxed-undefined))
           ((null? obj)(boxed-null))
           ((char? obj)(as-boxed-character obj))
           ((boolean? obj)(as-boxed-boolean obj))
           ((integer? obj)(as-boxed-integer obj))
           ((##flonum? obj)(as-boxed-float obj))
           ((##ratnum? obj)(as-boxed-ratio (numerator obj)(denominator obj)))
           ((symbol? obj)(as-boxed-symbol (symbol->string obj)))
           ((keyword? obj)(as-boxed-keyword (keyword->string obj)))
           ((string? obj)(as-boxed-text obj))
           (else (error (string-append "Can't convert to a boxed value"
                                       (object->string obj))))))



(c-define (c:make-integer i) (int) 
          scheme-object "make_integer" ""
          i)

(c-define (c:make-character ch) (char) 
          scheme-object "make_character" ""
          ch)

(c-define (c:make-float f) (float) 
          scheme-object "make_float" ""
          f)

(c-define (c:make-boolean b) (bool) 
          scheme-object "make_boolean" ""
          b)

(c-define (c:make-string str) (char-string) 
          scheme-object "make_string" ""
          (string-copy str))

(c-define (c:make-symbol str) (char-string) 
          scheme-object "make_symbol" ""
          (string->symbol str))

(c-define (c:make-keyword str) (char-string) 
          scheme-object "make_keyword" ""
          (string->keyword str))

;;; ---------------------------------------------------------------------
;;; object ids
;;; ---------------------------------------------------------------------

(c-define (c:bard-object->id obj) (scheme-object) 
          int "bard_object_to_id" ""
          (object->serial-number obj))

(c-define (c:bard-id->object id) (int) 
          scheme-object "bard_id_to_object" ""
          (serial-number->object id #f))

;;; ---------------------------------------------------------------------
;;; reading and loading
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

(c-define (c:bard-load-from-string str) (char-string) 
          scheme-object "bard_load_from_string" ""
          (cbard:load-from-string str))

(c-define (c:bard-eval obj) (scheme-object) 
          scheme-object "bard_eval" ""
          (cbard:eval obj))

;;; ---------------------------------------------------------------------
;;; operations on values
;;; ---------------------------------------------------------------------


(c-define (c:bard-get obj key) (scheme-object scheme-object) 
          scheme-object "bard_get" ""
          (%get obj key))

(c-define (c:bard-put obj key val) (scheme-object scheme-object scheme-object) 
          scheme-object "bard_put" ""
          (%put obj key val))

(c-define (c:bard-get-char-key obj key) (scheme-object char) 
          scheme-object "bard_get_char_key" ""
          (%frame-get obj key))

(c-define (c:bard-get-bool-key obj key) (scheme-object bool) 
          scheme-object "bard_get_bool_key" ""
          (%frame-get obj key))

(c-define (c:bard-get-int-key obj key) (scheme-object int) 
          scheme-object "bard_get_int_key" ""
          (%frame-get obj key))

(c-define (c:bard-get-float-key obj key) (scheme-object float) 
          scheme-object "bard_get_float_key" ""
          (%frame-get obj key))

(c-define (c:bard-get-symbol-key obj key) (scheme-object char-string) 
          scheme-object "bard_get_symbol_key" ""
          (%frame-get obj (string->symbol key)))

(c-define (c:bard-get-keyword-key obj key) (scheme-object char-string) 
          scheme-object "bard_get_keyword_key" ""
          (newline)(newline)(display key)
          (%frame-get obj (string->keyword key)))

(c-define (c:bard-get-string-key obj key) (scheme-object char-string) 
          scheme-object "bard_get_string_key" ""
          (%frame-get obj key))

(c-define (c:bard-get-keyword-symbol-or-string-key obj key) (scheme-object char-string) 
          scheme-object "bard_get_keyword_symbol_or_string_key" ""
          (%get-keyword-symbol-or-string-key obj key))

(c-define (c:bard-get-path obj path) (scheme-object char-string) 
          scheme-object "bard_get_path" ""
          (%get-path obj (%parse-slot-path path)))

(c-define (c:bard-keys obj) (scheme-object) 
          scheme-object "bard_keys" ""
          (%frame-keys obj))

(c-define (c:bard-length obj) (scheme-object) 
          int "bard_length" ""
          (length obj))

(c-define (c:bard-element obj index) (scheme-object int) 
          scheme-object "bard_element" ""
          (list-ref obj index))

(c-define (c:bard-cons obj1 obj2) (scheme-object scheme-object) 
          scheme-object "bard_cons" ""
          (cons obj1 obj2))

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(c-define (c:bard-display obj) (scheme-object) 
          void "bard_display" ""
          (display (object->string obj)))

(c-define (c:bard-newline) () 
          void "bard_newline" ""
          (newline))

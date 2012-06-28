;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard_c_api.scm
;;;; Project:       Bard
;;;; Purpose:       the C interface to the Bard interpreter
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(c-declare "#import <Foundation/Foundation.h>")

(c-define (c:bard-error?) () 
          bool "bard_error_status" ""
          (and $bard-error #t))

(c-define (c:bard-error) () 
          char-string "bard_error" ""
          $bard-error)

(c-define (c:clear-bard-error) () 
          void "clear_bard_error" ""
          (set! $bard-error #f))

(c-define (c:version) () 
          (pointer "NSString") "bard_version" ""
          (api:version))

(c-define (c:init-bard) () 
          bool "init_bard" ""
          (api:init-bard))

(c-define (c:bard-type obj) (scheme-object) 
          int "bard_type" ""
          (api:type-for-C obj))

(c-define (c:bard-read text) ((pointer "NSString")) 
          scheme-object "bard_read" ""
          (api:bard-read text))

(c-define (c:bard-eval expr) (scheme-object) 
          scheme-object "bard_eval" ""
          (api:bard-eval expr))

(c-define (c:bard-load text) ((pointer "NSString")) 
          bool "bard_load" ""
          (api:load-from-string text))

(c-define (c:bard-show expr) (scheme-object) 
          (pointer "NSString") "bard_show" ""
          (api:show expr))

(c-define (c:as-array expr) (scheme-object) 
          (pointer "NSMutableArray") "as_array" ""
          (api:as-array expr))

(c-define (c:as-integer expr) (scheme-object) 
          int "as_integer" ""
          (api:as-integer expr))

(c-define (c:as-dictionary expr) (scheme-object) 
          (pointer "NSMutableDictionary") "as_dictionary" ""
          (api:as-dictionary expr))



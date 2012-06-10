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

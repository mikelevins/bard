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

(c-define (c:bard-info path) ((pointer "NSString")) 
          (pointer "NSMutableDictionary") "bard_info" ""
          (let ((dirpath (objc:NSString->string path)))
            (api:bard-info dirpath)))

(c-define (c:bard-load-from-string text) ((pointer "NSString")) 
          bool "bard_load_from_string" ""
          (let ((load-string (objc:NSString->string text))) 
            (newline)
            (display "Loading from string: ")
            (%bard-load-from-string load-string)))



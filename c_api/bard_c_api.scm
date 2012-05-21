;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard_c_api.scm
;;;; Project:       Bard
;;;; Purpose:       the C interface to the Bard interpreter
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(c-define (c:version) () 
          char-string "version" ""
          (api:version))

(c-define (c:load path) (char-string) 
          void "bard_load" ""
          (api:load path))


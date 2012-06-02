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

(c-define (c:list-files path) ((pointer "NSString")) 
          (pointer "NSMutableArray") "list_files" ""
          (let ((dirpath (objc:NSString->string path)))
            (api:list-files dirpath)))



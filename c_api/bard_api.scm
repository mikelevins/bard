;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard_api.scm
;;;; Project:       Bard
;;;; Purpose:       Scheme API functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file defines Scheme API functions that are called by the C API
;;; functions defined as c-lambdas in bard_c_api.scm

(define (api:version)
  (objc:string->NSString $bard-version-string))

(define (api:list-files dirpath)
  (let ((files (directory-files dirpath))
        (arr (objc:make-NSMutableArray)))
    (for-each (lambda (f)
                (let ((nsstr (objc:string->NSString f)))
                  (objc:NSMutableArray/add-string! arr nsstr)))
              files)
    arr))

(define (api:count-files dirpath)
  (let* ((files (directory-files dirpath))
         (count (length files)))
    (objc:integer->NSNumber count)))


(define (api:bard-info dirpath)
  (let ((fr (%frame version: $bard-version-string
                    enabled: (%true)
                    arbitraryCount: 3
                    numberosity: 1.3)))
    (display "bard: ")
    (display (%as-string fr))
    (newline)
    (objc:frame->NSMutableDictionary fr)))





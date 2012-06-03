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
  (let ((files (directory-files dirpath)))
    (objc:list->NSMutableArray files)))

(define (api:load-file path)
  (with-exception-catcher (lambda (err)
                            (newline)
                            (display (string-append "Error loading file "
                                                    path " "))
                            (display-error err)
                            (newline)
                            #f)
                          (lambda ()(%bard-load path)
                            #t)))

(define (api:count-files dirpath)
  (let* ((files (directory-files dirpath))
         (count (length files)))
    (objc:integer->NSNumber count)))

(define (api:bard-info dirpath)
  (let ((fr (%frame version: $bard-version-string
                    enabled: (%true)
                    arbitraryCount: 3
                    numberosity: 1.3
                    things: (%list #f 3 2.4 "Apple"))))
    (objc:frame->NSMutableDictionary fr)))





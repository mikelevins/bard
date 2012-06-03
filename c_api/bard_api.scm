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

(define (api:init-bard)
  (let ((error-handler (lambda (err)
                         (display (error->string err))
                         #f))
        (initializer (lambda ()
                       (%init-bard)
                       #t)))
    (with-exception-catcher error-handler initializer)))

(define (api:bard-info dirpath)
  (let ((fr (%frame version: $bard-version-string
                    enabled: (%true)
                    arbitraryCount: 3
                    numberosity: 1.3
                    things: (%list #f 3 2.4 "Apple"))))
    (objc:frame->NSMutableDictionary fr)))


(define (api:bard-load-from-string load-string)
  (%bard-load-from-string load-string))


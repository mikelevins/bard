;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm -- experimental variation 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (vm:run vms)
  (let loop ()
    (if (%halted? vms)
        vms
        (begin
          (vm:fetch! vms)
          (vm:exec! vms)
          (loop)))))


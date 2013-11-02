;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          toplevel.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard toplevel
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; init
;;; ---------------------------------------------------------------------

(defun init-bard ()
  "Initialize the Bard toplevel environment"
  ;; built-in methods
  ;; ------------------------------------------------
  ;; call/cc
  (set-global! 'bard-symbols::|call/cc|
               (make-instance '<mfn> :name '|call/cc|
                              :args '(f) :code (assemble '((ARGS 1) (CC) (LVAR 0 0 ";" f)
                                                           (CALLJ 1)))))
  ;; exit
  (set-global! 'bard-symbols::|exit|
               (make-instance '<mfn> :name '|exit|
                              :args '(val) :code (assemble '((HALT))))))

;;; ---------------------------------------------------------------------
;;; bard toplevel
;;; ---------------------------------------------------------------------

(defparameter *the-bard-vm* nil)

(defparameter *bard-top-level*
  (bard-read-from-string
   "
(begin 

 (set! map
       (method (f ls)
               (if (<= (cons.length ls) 0)
                   nothing
                   (cons (f (cons.left ls))
                         (map f (cons.right ls))))))

 (set! load
       (method (url)
               (map (^ (exp) ((compiler exp)))
                    (stream.read-all-objects
                     (stream.create url 'character)))))

 (set! bard 
       (method () 
               (newline)
               (display \"bard> \")
               (write ((compiler (read))))
               (bard)))

 (bard))

"))

(defun bard ()
  (init-bard)
  (let ((vm (make-instance '<vm> :mfn (compiler *bard-top-level*))))
    (setf *the-bard-vm* vm)
    (vmrun vm)))

(defun bard-toplevel ()
  (format t "~%Bard version ~a~%~%" *bard-version-number*)
  (init-bard)
  (handler-case (let ((vm (make-instance '<vm> :mfn (compiler *bard-top-level*))))
                  (setf *the-bard-vm* vm)
                  (vmrun vm))
    (serious-condition (err)
      (format t "~%Unhandled error in the bard VM: ~S; terminating" err)
      (ccl::quit))))

;;; ---------------------------------------------------------------------
;;; building bard
;;; ---------------------------------------------------------------------

(defun build-bard (path)
  (let* ((out-dir (format nil "~a/bard-~a/" path *bard-version-number*))
         (out-path (merge-pathnames "bard" out-dir)))
    (ensure-directories-exist out-dir)
    (ccl::save-application out-path :toplevel-function 'bard-toplevel :prepend-kernel t)))

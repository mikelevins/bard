;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       bard system definition
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun bard-root () (make-pathname :directory (pathname-directory loadpath))))

(let* ((sysdefs (directory (merge-pathnames "**/*.asd" (bard-root))))
       (asdf-systems (mapcar (lambda (s) (make-pathname :directory (pathname-directory s))) sysdefs)))
  (dolist (s asdf-systems)
    (pushnew s asdf:*central-registry* :test 'equal)))

(defpackage "BARD.SYSTEM" (:use :cl :asdf))

(in-package "BARD.SYSTEM")

(defsystem bard
  :serial t
  :depends-on (:fset)
  :components
  ((:module src :serial t
            :components
            ((:file "packages")
             (:module runtime :serial t
                      :components
                      ((:module as :serial t
                                :components
                                ((:file "as")))
                       (:module functions :serial t
                                :components
                                ((:file "fn")
                                 (:file "functions")))
                       (:module cells :serial t
                                :components
                                ((:file "cell")))
                       (:module collections :serial t
                                :components
                                ((:file "sets")
                                 (:file "sequences")
                                 (:file "maps")
                                 (:file "maps-as-sequences")))))
             (:module reader :serial t
                      :components
                      ((:file "expressions")
                       (:file "read-table")
                       (:file "standard-readers")
                       (:file "standard-read-table")
                       (:file "read")))
             (:module compiler :serial t)
             (:module hlvm :serial t)
             (:module repl :serial t)))))

(in-package :cl-user)

(defun load-bard ()
  (asdf:oos 'asdf:load-op :bard)
  (funcall (intern "INIT-BARD-READ-TABLE" :bard)))

;;; (load-bard)
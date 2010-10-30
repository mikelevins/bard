;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          singleton.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       the singleton metaclass supports defining a class with
;;;;                only a single, unique instance
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

;(in-package "BARD")

;;; ========================================================================
;;; CLASS: singleton
;;; ========================================================================
;;; a class whose metaclass is singleton has a single unique instance

(defclass singleton-class (standard-class)
  ((instance :accessor instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)(superclass standard-class)) t)
(defmethod validate-superclass ((class singleton-class)(superclass singleton-class)) t)
(defmethod validate-superclass ((class standard-class)(superclass singleton-class)) nil)

(defmethod make-instance ((class singleton-class) &key)
  (unless (instance class)
    (setf (instance class) (call-next-method)))
  (instance class))

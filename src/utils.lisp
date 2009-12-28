;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       common utilities
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :util)

(defun plist->alist (plist)
  (if (null plist)
      plist
      (let ((k (car plist))
            (tl (cdr plist)))
        (if (null tl)
            (error "Odd number of arguments")
            (let ((v (car tl))
                  (tl (cdr tl)))
              (cons (cons k v)
                    (plist->alist tl)))))))
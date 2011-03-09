;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read.lisp
;;;; Project:       Bard
;;;; Purpose:       reader
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter *bard* nil)

(defparser bard-parser ((expression input) $1)
  ((input exp) $1)
  ;; application
  ((exp app) $1)
  ((app open-app exps close-app) (cons :app $2))
  ((app open-app close-app) (list :app))
  ((open-app :open-app))
  ((close-app :close-app))
  ;; vals
  ((exp vals) $1)
  ((vals open-vals exps close-vals) (cons :vals $2))
  ((vals open-vals close-vals) (list :vals))
  ((open-vals :open-vals))
  ((close-vals :close-vals))
  ;; maps
  ((exp map) $1)
  ((map open-map exps close-map) (cons :map $2))
  ((map open-map close-map) (list :map))
  ((open-map :open-map))
  ((close-map :close-map))
  ;; expression sequences
  ((exps exp) (list $1))
  ((exps exp exps) (cons $1 $2))
  ;; text
  ((exp :text) (list :text $1))
  ;; characters
  ((exp character) (list :character $1))
  ((character :backslash :token) $2)
  ;; tokens
  ((exp :token) (list :token $1)))

;;; (bard::test-parse)

(defparameter $test-strings
  '("\"test text\""
    "foo" ":bar" "bard.core:foo" "*" "bard.core:+"
    "\\F" "\\." "\\b" "\\space"
    "()" "(foo)" "(+ :foo bard.core:bar)" "(* (+ foo bar))"
    "[]" "[0 1 2]" "[[]]" "[1 [2 3]]"
    "{}" "{1 2 3 4}" "{:name \"Fred\" :age 45 :things [1 2 3]}"
    ))

(defun test-parse ()
  (format t "~%bard parser test~%")
  (dolist (inp $test-strings)
    (format t "   input: ~S;~%"  inp)
    (format t "  output: ~S~%~%" (parse-expr inp))))

(defun make-bard-lexer (s)
  (with-input-from-string (inp s)
    (let ((lexer (make-instance 'lexer-input-stream
                                :stream inp
                                :rules '(("[ \\t\\n]*'[ \\t\\n]*" . :quote)
                                         ("[ \\t\\n]*,[ \\t\\n]*" . :unquote)
                                         ("[ \\t\\n]*\\" . :backslash)
                                         ("[ \\t\\n]*\\([ \\t\\n]*" . :open-app)
                                         ("[ \\t\\n]*\\)[ \\t\\n]*" . :close-app)
                                         ("[ \\t\\n]*\\[[ \\t\\n]*" . :open-vals)
                                         ("[ \\t\\n]*\\][ \\t\\n]*" . :close-vals)
                                         ("[ \\t\\n]*\\{[ \\t\\n]*" . :open-map)
                                         ("[ \\t\\n]*\\}[ \\t\\n]*" . :close-map)
                                         ("[ \\t\\n]*\"[^\"]*\"[ \\t\\n]*" . :text)
                                         ("[ \\t\\n]*[~!@$%^&*_\\-\\+=0-9a-zA-Z:\\.<>?/]+[ \\t\\n]*" . :token)))))
      (lambda ()(stream-read-token lexer)))))

(defun parse-expr (str)
  (bard-parser (make-bard-lexer str)))

(defun construct-character (parse-params bard)
  (make-instance 'character :data (find-character-data (first parse-params))))

(defun construct-text (parse-params)
  (make-instance 'text :data (first parse-params)))

(defun parse-token (parse-params bard)
  )

(defun parse-vals (parse-params bard)
  )

(defun parse-map (parse-params bard)
  )

(defun parse-app (parse-params bard)
  )

;;; possible parse-tree outputs:
;;; :character
;;; :text
;;; :token
;;; :vals
;;; :map
;;; :app

(defun read-expr (str bard)
  (let* ((parse-tree (parse-expr str))
         (parse-class (first parse-tree))
         (parse-params (rest parse-tree)))
    (ecase parse-class
      ((:character)(construct-character parse-params bard))
      ((:text)(construct-text parse-params))
      ((:token)(parse-token parse-params bard))
      ((:vals)(parse-vals parse-params bard))
      ((:map)(parse-map parse-params bard))
      ((:app)(parse-app parse-params bard)))))


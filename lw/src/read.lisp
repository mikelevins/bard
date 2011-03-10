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
  ((exp :text) (list :text (subseq $1 1 (1- (length $1)))))
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

;;; (parse-expr "{1 2}")

(defun construct-character (parse-params bard)
  (make-instance 'character :data (find-character-data (first parse-params))))

(defun construct-text (parse-params)
  (make-instance 'text :elements (first parse-params)))

(defun parse-as-float (tok)
  (let* ((lexer (with-input-from-string (inp tok)
                  (make-instance 'lexer-input-stream
                                 :stream inp
                                 :rules '(("[ \\t\\n]*[-+]?[0-9]+\\.[0-9]+[ \\t\\n\\(\\[\\{]*" . :float))))))
    (multiple-value-bind (type val)
        (handler-case (stream-read-token lexer)
          (condition (err) (values nil nil)))
      (when val (read-from-string val)))))

;;(parse-as-float "-1.2")

(defun parse-as-int (tok)
  (let* ((lexer (with-input-from-string (inp tok)
                  (make-instance 'lexer-input-stream
                                 :stream inp
                                 :rules '(("[ \\t\\n]*[-+]?[0-9]+[ \\t\\n\\(\\[\\{]*" . :int))))))
    (multiple-value-bind (type val)
        (handler-case (stream-read-token lexer)
          (condition (err) (values nil nil)))
      (when val (read-from-string val)))))

;;(parse-as-int "-1234567")

(defun module-name-start? (s)
  (and (stringp s)
       (not (zerop (length s)))
       (every #'alpha-char-p s)
       (every #'lower-case-p s)))

;;; (module-name-start? "foo")

(defun module-name-extension? (s)
  (and (stringp s)
       (not (zerop (length s)))
       (every #'alphanumericp s)
       (every #'lower-case-p s)))

(defun validate-module-name (mnm)
  (if (string= "" mnm)
      mnm
   (let ((name-parts (split-sequence #\. mnm)))
     (if (null name-parts)
         nil
         (and (module-name-start? (first name-parts))
              (every #'module-name-extension? (rest name-parts))
              mnm)))))

;;; (validate-module-name "foo")

(defun parse-name-token (tok)
  (let* ((name-parts (split-sequence #\: tok))
         (part-count (length name-parts)))
    (cond
      ((= part-count 1)
       (let ((mname nil)
             (vname (first name-parts)))
         (if (zerop (length vname))
             (error "Malformed name: ~S" tok)
             (list :name nil vname))))
      ((= part-count 2)
       (let ((mname (validate-module-name (first name-parts)))
             (vname (second name-parts)))
         (if mname
             (if (zerop (length vname))
                 (error "Malformed name: ~S" tok)
                 (if (zerop (length mname))
                     (list :name "bard.keyword" vname)
                     (list :name mname vname)))
             (error "Malformed name: ~S" tok))))
      (t (error "Malformed name: ~S" tok)))))

(defun parse-as-name (tok bard)
  (let* ((name-spec (parse-name-token tok))
         (vname (third name-spec))
         (mname (second name-spec)))
    (make-instance 'name :module-name mname :variable-name vname)))

;;;(setq $lex (make-name-lexer "foo"))
;;;(funcall $lex)
;;;(parse-as-name "foo" nil)
;;;(parse-as-name ":bar" nil)
;;;(parse-as-name "foo.baz:bar" nil)

(defun parse-token (tok bard)
  (cond
    ((string= tok "undefined")(make-instance 'undefined))
    ((string= tok "nothing")(make-instance 'nothing))
    ((string= tok "true")(make-instance 'true))
    ((string= tok "false")(make-instance 'false))
    (t (let ((float-parse (parse-as-float tok))
             (int-parse (parse-as-int tok)))
         (cond
           (float-parse (make-instance 'float :data float-parse))
           (int-parse (make-instance 'integer :data int-parse))
           (t (parse-as-name tok bard)))))))

(defun parse-vals (parse-params bard)
  (let* ((vals (mapcar #'(lambda (p)(construct-value p bard))
                       parse-params)))
    (make-instance 'sequence :elements (fset:convert 'fset:seq vals))))

(defun parse-map (parse-params bard)
  (if (evenp (length parse-params))
      (let* ((vals (mapcar #'(lambda (p)(construct-value p bard))
                           parse-params)))
        (make-instance 'map :entries vals))
      (error "Invalid init data for a map: ~S" parse-params)))

(defun parse-app (parse-params bard)
  (let* ((vals (mapcar #'(lambda (p)(construct-value p bard))
                       parse-params)))
    (make-instance 'application :elements (fset:convert 'fset:seq vals))))

;;; possible parse-tree outputs:
;;; :character
;;; :text
;;; :token
;;; :vals
;;; :map
;;; :app

(defun construct-value (parse-tree bard)
  (let* ((parse-class (first parse-tree))
         (parse-params (rest parse-tree)))
    (ecase parse-class
      ((:character)(construct-character parse-params bard))
      ((:text)(construct-text parse-params))
      ((:token)(parse-token (first parse-params) bard))
      ((:vals)(parse-vals parse-params bard))
      ((:map)(parse-map parse-params bard))
      ((:app)(parse-app parse-params bard)))))

(defun read-expr (str bard)
  (let ((parse-tree (parse-expr str)))
    (construct-value parse-tree bard)))

;;; (setq $bard (make-instance 'bard-runtime))
;;; (read-expr "undefined" $bard)
;;; (read-expr "\"foo bar\"" $bard)

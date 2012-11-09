;;;; bard.lisp

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; reader
;;; ---------------------------------------------------------------------

(defparameter $bard-character-names (make-hash-table :test #'equal))

(defun def-bard-character-name (name ch)
  (setf (gethash name $bard-character-names) ch))

(defun init-bard-character-names ()
  (def-bard-character-name "space" #\space)
  (def-bard-character-name "tab" #\tab)
  (def-bard-character-name "newline" #\newline))

(init-bard-character-names)

(defun bard-character-name->character (nm)
  (gethash nm $bard-character-names nil))

(defun standard-lisp-readtable ()
  ccl::%standard-readtable%)

(defun read-square-bracket (stream char)
  (let ((expr (cl:read-delimited-list #\] stream nil)))
    (list->bard-list expr)))

(defun read-curly-bracket (stream char)
  (let ((expr (cl:read-delimited-list #\} stream nil)))
    (list->bard-table expr)))

(defun bard-character-delimiter? (ch)
  (or (not (standard-char-p ch))
      (member ch '(#\" #\' #\( #\) #\` #\: #\; #\# #\[ #\] #\{ #\} #\| #\, #\space #\tab #\newline) :test #'char=)))

(defun bard-single-character-input? (ch0 ch1)
  (or (null ch1)
      (bard-character-delimiter? ch1)))

(defun bard-unicode-character-input? (ch0 ch1)
  (and (or (char= ch0 #\u)
           (char= ch0 #\U))
       (char= ch1 #\+)))

(defun bard-named-character-input? (ch0 ch1)
  (not (bard-character-delimiter? ch1)))

(defmethod bard-unicode-code->character ((code integer))
  (code-char code))

(defmethod bard-unicode-code->character ((code string))
  (bard-unicode-code->character (parse-integer code :radix 16)))

(defun bard-read-unicode-character (stream)
  (let ((chars '()))
    (block reading
      (loop for ch = (read-char stream nil nil nil) then (read-char stream nil nil nil)
         do (if (or (null ch)
                    (bard-character-delimiter? ch))
                (return-from reading
                  (bard-unicode-code->character (coerce (reverse chars) 'string)))
                (push ch chars))))))

(defun bard-read-named-character (chars stream)
  (let ((chars (reverse chars)))
    (block reading
      (loop for ch = (read-char stream nil nil nil) then (read-char stream nil nil nil)
         do (if (or (null ch)
                    (bard-character-delimiter? ch))
                (return-from reading
                  (bard-character-name->character (coerce (reverse chars) 'string)))
                (push ch chars))))))

(defun read-bard-character (stream char)
  (let* ((ch0 (read-char stream))
         (ch1 (peek-char nil stream nil nil nil)))
    (cond
      ((bard-single-character-input? ch0 ch1) ch0)
      ((bard-unicode-character-input? ch0 ch1)(bard-read-unicode-character stream))
      ((bard-named-character-input? ch0 ch1)(bard-read-named-character (list ch0) stream))
      (t (error "Invalid character syntax")))))

(defparameter +bard-readtable+
  (let ((brt (copy-readtable)))
    (setf (readtable-case brt) :preserve)
    (set-macro-character #\[ #'read-square-bracket nil brt)
    (set-macro-character #\] (get-macro-character #\) nil) nil brt)
    (set-macro-character #\{ #'read-curly-bracket nil brt)
    (set-macro-character #\} (get-macro-character #\) nil) nil brt)
    (set-macro-character #\\ #'read-bard-character nil brt)
    brt))

(defmethod bard-read ((in stream))
  (let ((*readtable* +bard-readtable+))
    (cl:read in nil nil nil)))

(defmethod bard-read ((instr string))
  (with-input-from-string (in instr)
    (bard-read in)))

;;; ---------------------------------------------------------------------
;;; evaluator
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; printer
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; driver and repl
;;; ---------------------------------------------------------------------

(defmethod bard ((in stream))
  (let ((*readtable* +bard-readtable+))
    (bard-print (bard-eval (bard-read in) *bard-environment*) *bard-standard-output*)))

(defmethod bard ((instr string))
  (with-input-from-string (in instr)
    (bard in)))

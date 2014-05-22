;;;;**************************************************************************
;;;;FILE:               reader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the Common Lisp Reader.
;;;;    
;;;;    We implement a Common Lisp Reader to be able to read lisp sources.
;;;;    First, we implement a complete standard compliant lisp reader,
;;;;    with additionnal hooks (token parser).
;;;;
;;;;    A READTABLE-PARSE-TOKEN function takes a TOKEN as argument, and
;;;;    must return two values:
;;;;     - A boolean indicating whether the it could parse the token,
;;;;     - a parsed lisp object it could, or an error message (string) if not.
;;;;
;;;;    See also the TOKEN functions, CONSTITUENT-TRAIT, SYNTAX-TABLE and
;;;;    CHARACTER-DESCRIPTION...
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-08-26 <PJB> Corrected bugs reading "||", "( ;comment )" and "#C(123 456)".
;;;;    2007-03-04 <PJB> Extracted from source.lisp
;;;;BUGS
;;;;    When we've reached the end of the stream, if we (read stream nil)
;;;;    it goes on an infinite loop.
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2009
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(CL:IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.READER"
  (:nicknames "READER")
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.SOURCE-FORM")
  (:shadow "READTABLE"
           "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
           "READ" "READ-PRESERVING-WHITESPACE"
           "READ-DELIMITED-LIST"
           "READ-FROM-STRING"
           "READTABLE-CASE" "READTABLEP"
           "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
           "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
           "SET-SYNTAX-FROM-CHAR"
           "WITH-STANDARD-IO-SYNTAX"
           "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
           "*READ-SUPPRESS*" "*READTABLE*")
  (:EXPORT "READTABLE"
           "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
           "READ" "READ-PRESERVING-WHITESPACE"
           "READ-DELIMITED-LIST"
           "READ-FROM-STRING"
           "READTABLE-CASE" "READTABLEP"
           "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
           "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
           "SET-SYNTAX-FROM-CHAR"
           "WITH-STANDARD-IO-SYNTAX"
           "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
           "*READ-SUPPRESS*" "*READTABLE*"
           ;; Extensions:
           "READTABLE-SYNTAX-TABLE" "READTABLE-PARSE-TOKEN"
           "SET-INDIRECT-DISPATCH-MACRO-CHARACTER" 
           "SET-INDIRECT-MACRO-CHARACTER"
           "LIST-ALL-MACRO-CHARACTERS"
           "SIMPLE-READER-ERROR" "SIMPLE-END-OF-FILE")
  (:DOCUMENTATION
   "This package implements a standard Common Lisp reader.

    Copyright Pascal J. Bourguignon 2006 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.READER")



(define-condition simple-reader-error (simple-error reader-error) ())
(define-condition simple-end-of-file  (simple-error end-of-file)  ())


(defun serror (condition stream control-string &rest arguments)
  (error condition
         :stream stream
         :format-control control-string
         :format-arguments arguments))


;; (LET ((*READTABLE* (COPY-READTABLE NIL)))
;;   (SET-DISPATCH-MACRO-CHARACTER
;;    #\# #\. (LAMBDA (&REST ARGS) ARGS)))
;; ;; (setf (readtable-case *readtable*) :preserve)
;; (let ((*readtable* (copy-readtable)))
;;   ;; Quick and dirty disable : --> read three or four tokens
;;   ;; for pack:sym or pack::sym
;;   (set-macro-character #\: (lambda (stream char) #\:) nil)
;;   (SAFE-TEXT-FILE-TO-STRING-LIST path))
;;
;;
;; (defun unnamed-char-p (ch)
;;   (not (null (regexp:match "^U\\([0-9A-F]\\{4\\}\\|[0-9A-F]\\{8\\}\\)$"
;;                            (char-name ch)))))
;;
;;
;; (defun collect-chars (&key (start 0) (end #x11000) name)
;;   (loop
;;      :with table = (make-hash-table :test (function equalp))
;;      :for code :from start :below end
;;      :for char = (code-char code)
;;      :for name = (char-name char)
;;      :do (unless (unnamed-char-p char)
;;            (dolist (word (regexp:regexp-split "[-_]" name))
;;              (push char (gethash word table nil))))
;;      :finally (return table)))



;;----------------------------------------

(defclass character-description ()
  ((syntax   :reader character-syntax
             :initarg :syntax)
   (traits   :reader character-constituent-traits
             :initarg :traits   :initform nil)
   (macro    :reader character-macro
             :initarg :macro    :initform nil
             :documentation "A macro character function.")
   (dispatch :reader character-dispatch
             :initarg :dispatch :initform nil
             :documentation "A HASH-TABLE character -> dmc function."))
  (:documentation
   "
Description of one character. 

In the syntax tables, a single character description instance can be
shared by several characters, but with copy-on-write.
"))

;; macro-character-function
;; dispatch-macro --> map character -> dispatch-macro-character-function


(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Character syntaxes:
  (defconstant +cs-invalid+                         0)
  (defconstant +cs-whitespace+                      1)
  (defconstant +cs-single-escape+                   2)
  (defconstant +cs-multiple-escape+                 3)
  (defconstant +cs-constituent+                     4)
  (defconstant +cs-terminating-macro-character+     5)
  (defconstant +cs-non-terminating-macro-character+ 6)


;;; Constituent traits:
  (defconstant +ct-invalid+                        #b00000000000001)
  (defconstant +ct-alphabetic+                     #b00000000000010)
  (defconstant +ct-digit+                          #b00000000000100)
  (defconstant +ct-alphadigit+                     #b00000000000110)
  (defconstant +ct-package-marker+                 #b00000000001000)
  (defconstant +ct-plus-sign+                      #b00000000010000)
  (defconstant +ct-minus-sign+                     #b00000000100000)
  (defconstant +ct-sign+                           #b00000000110000)
  (defconstant +ct-dot+                            #b00000001000000)
  (defconstant +ct-decimal-point+                  #b00000010000000)
  (defconstant +ct-ratio-marker+                   #b00000100000000)
  (defconstant +ct-float-exponent-marker+          #b00001000000000)
  (defconstant +ct-short-float-exponent-marker+    #b00011000000000)
  (defconstant +ct-single-float-exponent-marker+   #b00101000000000)
  (defconstant +ct-double-float-exponent-marker+   #b01001000000000)
  (defconstant +ct-long-float-exponent-marker+     #b10001000000000)
  (defconstant +ct-max+ +ct-long-float-exponent-marker+)
  ) ;;eval-when


(deftype constituent-trait () `(integer 0 ,(expt 2 (integer-length  +ct-max+))))


(declaim (inline traitp))
(defun traitp (trait traits)
  "Returns whether the TRAIT is in the TRAITS 'set'."
  (plusp (logand trait traits)))


;;; The shared character descriptions:

(defparameter *cd-invalid*                (make-instance 'character-description
                                            :syntax +cs-invalid+
                                            :traits +ct-invalid+))
(defparameter *cd-whitespace*             (make-instance 'character-description
                                            :syntax +cs-whitespace+
                                            :traits +ct-invalid+))
(defparameter *cd-constituent-invalid*    (make-instance 'character-description
                                            :syntax +cs-whitespace+
                                            :traits +ct-invalid+))
(defparameter *cd-constituent-alphabetic* (make-instance 'character-description
                                            :syntax +cs-constituent+
                                            :traits +ct-alphabetic+))

;; ----------------------------------------

(defclass syntax-table ()
  (standard-characters
   extended-characters
   constituent
   invalid)
  (:documentation
   "
STANDARD-CHARACTERS is a vector of CHARACTER-DESCRIPTION instances
for the standard character codes below +STANDARD-CHARACTERS-LIMIT+.

EXTENDED-CHARACTERS is NIL, or a HASH-TABLE mapping characters to
CHARACTER-DESCRIPTIONS instances for the extended characters with
codes above +STANDARD-CHARACTERS-LIMIT+.

Extended characters without an entry in EXTENDED-CHARACTERS either
have CONSTITUENT or INVALID CHARACTER-DESCRIPTION, depending on whether
they're GRAPHIC-CHAR-P or not.
"))

(defconstant +standard-characters-limit+ 128)


(defmethod initialize-instance
    :after ((self syntax-table) &key &allow-other-keys)
  (let ((table        (make-array +standard-characters-limit+
                                  :initial-element *cd-invalid*)))
    (setf (aref table (char-code #\Backspace)) *cd-constituent-invalid*
          (aref table (char-code #\Rubout))    *cd-constituent-invalid*
          (aref table (char-code #\Tab))       *cd-whitespace*
          (aref table (char-code #\Newline))   *cd-whitespace*
          (aref table (char-code #\Linefeed))  *cd-whitespace*
          (aref table (char-code #\Page))      *cd-whitespace*
          (aref table (char-code #\Return))    *cd-whitespace*
          (aref table (char-code #\Space))     *cd-whitespace*)
    (loop
       :for chdesc
       :in '((#.+cs-terminating-macro-character+ "\"'(),;`"
              #.+ct-alphabetic+)
             (#.+cs-non-terminating-macro-character+ "#"
              #.+ct-alphabetic+)
             (#.+cs-single-escape+ "\\"
              #.+ct-alphabetic+)
             (#.+cs-multiple-escape+ "|"
              #.+ct-alphabetic+)
             (#.+cs-constituent+ "!$%&*<=>?@[]^_{}~"
              #.+ct-alphabetic+)
             (#.+cs-constituent+ ":"
              #.+ct-package-marker+)
             (#.+cs-constituent+ "+"
              #.+ct-alphabetic+ #.+ct-plus-sign+)
             (#.+cs-constituent+ "-"
              #.+ct-alphabetic+ #.+ct-minus-sign+)
             (#.+cs-constituent+ "."
              #.+ct-alphabetic+ #.+ct-dot+ #.+ct-decimal-point+)
             (#.+cs-constituent+ "/"
              #.+ct-alphabetic+ #.+ct-ratio-marker+)
             (#.+cs-constituent+ "0123456789"
              #.+ct-alphadigit+)
             (#.+cs-constituent+ "Dd"
              #.+ct-alphadigit+ #.+ct-double-float-exponent-marker+)
             (#.+cs-constituent+ "Ee"
              #.+ct-alphadigit+ #.+ct-float-exponent-marker+)
             (#.+cs-constituent+ "Ff"
              #.+ct-alphadigit+ #.+ct-single-float-exponent-marker+)
             (#.+cs-constituent+ "Ll"
              #.+ct-alphadigit+ #.+ct-long-float-exponent-marker+)
             (#.+cs-constituent+ "Ss"
              #.+ct-alphadigit+ #.+ct-short-float-exponent-marker+)
             (#.+cs-constituent+ "ABCGHIJKMNOPQRTUVWXYZabcghijkmnopqrtuvwxyz"
              #.+ct-alphadigit+))
       :do (loop
              :with desc = (make-instance 'character-description
                             :syntax (first chdesc)
                             :traits (if (null (cdddr chdesc))
                                         (third chdesc)
                                         (apply (function logior)
                                                (cddr chdesc))))
              :for ch :across (second chdesc)
              :do (setf (aref table (char-code ch)) desc)))
    (setf (slot-value self 'standard-characters) table
          (slot-value self 'extended-characters) nil))
  self)

(defgeneric copy-syntax-table (syntax-table))
(defgeneric character-description (syntax-table character))

(defmethod copy-syntax-table ((self syntax-table))
  (let ((copy (make-instance 'syntax-table)))
    (setf (slot-value copy 'standard-characters)
          (copy-seq (slot-value self 'standard-characters))
          (slot-value copy 'extended-characters)
          (and (slot-value self 'extended-characters)
               (copy-hash-table (slot-value self 'extended-characters))))
    copy))

(defmethod character-description ((self syntax-table) (ch character))
  (let ((code (char-code ch)))
    (if (< code +standard-characters-limit+)
        (aref (slot-value self 'standard-characters) code)
        (or (and (slot-value self 'extended-characters)
                 (gethash code (slot-value self 'extended-characters)))
            (if (graphic-char-p ch)
                *cd-constituent-alphabetic*
                *cd-invalid*)))))

(defgeneric (setf character-description) (val syntax-table character))
(defmethod (setf character-description) (val (self syntax-table) (ch character))
  (let ((code (char-code ch)))
    (if (< code +standard-characters-limit+)
        (setf (aref (slot-value self 'standard-characters) code) val)
        (progn
          (unless (slot-value self 'extended-characters)
            (setf (slot-value self 'extended-characters) (make-hash-table)))
          (setf  (gethash code (slot-value self 'extended-characters)) val)))))

;;----------------------------------------

(defvar *standard-readtable*         nil
  "Only used by SET-SYNTAX-FROM-CHAR")

(defvar *readtable*                  nil
  "
The value of *READTABLE* is called the current readtable. It controls
the parsing behavior of the Lisp reader, and can also influence the
Lisp printer (e.g., see the  function READTABLE-CASE).

URL: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm
")

(defvar *read-base*                  10
  "
Controls the interpretation of tokens by READ as being integers or
ratios.

The value of *READ-BASE*, called the current input base, is the radix
in which  integers and ratios are to be read by the Lisp reader. The
parsing of other numeric  types (e.g., floats) is not affected by this
option.

The effect of *READ-BASE* on the reading of any particular rational
number can be locally overridden by explicit use of the #O, #X, #B, or
#nR syntax or by a trailing decimal point.

URL: http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_bas.htm
")

(defvar *read-eval*                  t
  "
If it is true, the #. reader macro has its normal effect. Otherwise,
that reader macro signals an error of type reader-error.

URL: http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_eva.htm
")

(defvar *read-suppress*              nil
  "
This variable is intended primarily to support the operation of the
read-time conditional notations #+ and #-. If it is false, the Lisp
reader operates normally.  If the value of *read-suppress* is true,
read, read-preserving-whitespace,  read-delimited-list, and
read-from-string all return a primary value of nil when they complete
successfully.

URL: http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_sup.htm
")

(defvar *READ-DEFAULT-FLOAT-FORMAT* 'single-float
  "
Controls the floating-point format that is to be used when reading a
floating-point number that has no exponent marker or that has e or E
for an exponent marker. Other  exponent markers explicitly prescribe
the floating-point format to be used.

The printer uses *read-default-float-format* to guide the choice of
exponent markers when printing floating-point numbers.

URL: http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_def.htm
")


(declaim (ftype (function (t) t) parse-token))

(defclass readtable ()
  ((case          :initarg :case
                  :initform :upcase
                  :type (member :upcase :downcase :preserve :invert))
   (syntax-table  :accessor readtable-syntax-table
                  :initarg :syntax-table
                  :initform (make-instance 'syntax-table))
   (parse-token   :accessor readtable-parse-token
                  :initarg :parse-token
                  :initform (function parse-token)))
  (:documentation
   "
A READTABLE maps characters into syntax types for the Lisp reader; see
Section 2 (Syntax). A readtable also contains associations between
macro characters and their  reader macro functions, and records
information about the case conversion rules to be used by the Lisp
reader when parsing symbols.

Each simple character must be representable in the readtable. It is
implementation-defined whether non-simple characters can have syntax
descriptions in the readtable.

URL: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm
"))



(defun copy-readtable (&optional (from-readtable *readtable*) (to-readtable nil))
  (if (null from-readtable)
      (if (null to-readtable)
          (make-instance 'readtable)
          (progn
            (setf (readtable-case to-readtable) :upcase
                  (readtable-syntax-table to-readtable) (make-instance
                                                            'syntax-table)
                  (readtable-parse-token to-readtable)  (function parse-token))
            to-readtable))
      (if (null to-readtable)
          (make-instance 'readtable
            :case (readtable-case from-readtable)
            :syntax-table (copy-syntax-table
                           (readtable-syntax-table from-readtable))
            :parse-token (readtable-parse-token from-readtable))
          (progn
            (setf (readtable-case to-readtable) (readtable-case from-readtable)
                  (readtable-syntax-table to-readtable) (copy-syntax-table
                                                         (readtable-syntax-table
                                                          from-readtable))
                  (readtable-parse-token to-readtable)  (readtable-parse-token
                                                         from-readtable))
            to-readtable))))

(defun reader-dispatch-macro-error-undefined (stream ch sub-char)
  (serror 'simple-reader-error stream
          "After #\\~A is #\\~A an undefined dispatch macro character"
          ch sub-char))

(defun reader-dispatch-macro-error-invalid (stream sub-char arg)
  (declare (ignore sub-char arg))
  (serror 'simple-reader-error stream
          "objects printed as # in view of *PRINT-LEVEL* cannot be read back in"))


(defun reader-macro-dispatch-function (stream ch)
  (let* ((arg  (loop
                  :for ch = (read-char stream t nil t)
                  :while (digit-char-p ch)
                  :collect ch :into digits
                  :finally (unread-char ch stream)
                  (return (when digits
                            (parse-integer (coerce digits 'string))))))
         (sub-char (read-char stream t nil t))
         (cd (character-description (readtable-syntax-table *readtable*) ch))
         (fun (gethash (char-upcase sub-char) (character-dispatch cd))))
    (if fun
        (funcall fun stream  arg sub-char)
        (reader-dispatch-macro-error-undefined stream ch sub-char))))



(defgeneric process-case-function (mode)
  (:method ((mode (eql :preserve))) (function identity))
  (:method ((mode (eql :downcase))) (function char-downcase))
  (:method ((mode (eql :upcase)))   (function char-upcase))
  (:method ((mode (eql :invert)))
    (lambda (ch)
      (cond ((upper-case-p ch) (char-downcase ch))
            ((lower-case-p ch) (char-upcase   ch))
            (t                                ch)))))


;;; For tokens we need to keep track of the characters and their
;;; traits in parallel:

(declaim (inline make-token  token-text token-traits
                 token-length token-char token-char-traits
                 token-collect-character))
(defun make-token ()
  (declare (inline arr))
  (flet ((arr (type)
           (make-array 8 :adjustable t :fill-pointer 0 :element-type type)))
    (cons (arr 'character) (arr 'constituent-trait))))
(defun token-text        (token)       (car token))
(defun token-traits      (token)       (cdr token))
(defun token-length      (token)       (length (car token)))
(defun token-char        (token index) (aref (car token) index))
(defun token-char-traits (token index) (aref (cdr token) index))
(defun token-collect-character (token character traits)
  (vector-push-extend  character (car token))
  (vector-push-extend  traits    (cdr token)))

(defun token-delimiter-p (character)
  (let ((cs (character-syntax
             (character-description (readtable-syntax-table *readtable*)
                                    character))))
    (or (= cs +cs-whitespace+) (= cs +cs-terminating-macro-character+))))


(defvar *references* nil "Used to implement #= and ##.")


(defun read-token (input-stream eof-error-p eof-value recursive-p
                   preserve-whitespace-p first-char readtable)
  "
DO:             Implements parts of READ and READ-PRESERVING-WHITESPACE.

INPUT-STREAM:   The stream that is read.
EOF-ERROR-P:    Whether we should signal an END-OF-FILE error upon EOF.
EOF-VALUE:      Unless EOF-ERROR-P, the value to be returned in case of EOF.
RECURSIVE-P:    Whether the read is recursive.
                The *reference* table is reset only when RECURSIVE-P is false.
PRESERVE-WHITESPACE-P:
                Whether a terminating whitespace will be unread or not.
FIRST-CHAR:     NIL or a CHARACTER that is used first, before reading the stream.
                This should be faster than UNREAD-CHAR it, and foremost, it allows
                for two unread character, this FIRST-CHAR plus an actual UNREAD-CHAR one.
READTABLE:      The readtable to use.

RETURN:         tokenp == t    ; a token.  Or
                tokenp == :EOF ; the eof-value.  Or
                tokenp == NIL  ; a list of values read.
"
  (macrolet ((unless-eof (place &body body)
               `(cond
                  (,place      ,@body)
                  (eof-error-p (serror 'simple-end-of-file input-stream
                                       "input stream ~S has reached its end"
                                       input-stream))
                  (t       (return-from read-token (values :eof eof-value)))))
             (error-invalid-character (ch)
               `(serror 'simple-reader-error input-stream
                        "invalid character #\\~A" ,ch)))
    (let ((*references* (if recursive-p
                            *references*
                            (make-hash-table))))
      (prog (x y
             (token (make-token))
             (syntax-table (readtable-syntax-table readtable))
             (procase (process-case-function (readtable-case readtable))))
       :begin
       (setf x (or first-char (read-char input-stream nil nil t))
             first-char nil)
       (unless-eof x
         (let ((cd (character-description syntax-table x)))
           (ecase (character-syntax cd)
             ((#.+cs-invalid+)
              (error-invalid-character x))
             ((#.+cs-whitespace+)
              (go :begin))
             ((#.+cs-single-escape+)
              (let ((z (read-char input-stream nil nil t)))
                (unless-eof z
                  (token-collect-character token z +ct-alphabetic+)))
              (go :collect-token))
             ((#.+cs-multiple-escape+)
              (go :collect-multiple-escape-token))
             ((#.+cs-constituent+)
              (token-collect-character token (funcall procase x)
                                       (character-constituent-traits cd))
              (go :collect-token))
             ((#.+cs-terminating-macro-character+
               #.+cs-non-terminating-macro-character+)
              ;; If the macro returns no value, the caller will
              ;; have to call us again, or not: (#-(and)x)
              (return-from read-token
                (values nil (multiple-value-list
                             (funcall (get-macro-character x readtable)
                                      input-stream x))))))))
       :collect-token
       (setf y (read-char input-stream nil nil t))
       (if y
           (let ((cd (character-description syntax-table y)))
             (ecase (character-syntax cd)
               ((#.+cs-invalid+)
                (error-invalid-character y))
               ((#.+cs-whitespace+)
                (when preserve-whitespace-p
                  (unread-char y input-stream))
                (go :parse-token))
               ((#.+cs-single-escape+)
                (let ((z (read-char input-stream nil nil t)))
                  (unless-eof z
                    (token-collect-character token z +ct-alphabetic+)))
                (go :collect-token))
               ((#.+cs-multiple-escape+)
                (go :collect-multiple-escape-token))
               ((#.+cs-constituent+
                 #.+cs-non-terminating-macro-character+)
                (token-collect-character token (funcall procase y)
                                         (character-constituent-traits cd))
                (go :collect-token))
               ((#.+cs-terminating-macro-character+)
                (unread-char y input-stream)
                (go :parse-token))))
           (go :parse-token))
       :collect-multiple-escape-token
       (setf y (read-char input-stream nil nil t))
       (unless-eof y
         (let ((cd (character-description syntax-table y)))
           (ecase (character-syntax cd)
             ((#.+cs-invalid+)
              (error-invalid-character y))
             ((#.+cs-single-escape+)
              (let ((z (read-char input-stream nil nil t)))
                (unless-eof z
                  (token-collect-character token z +ct-alphabetic+)))
              (go :collect-multiple-escape-token))
             ((#.+cs-multiple-escape+)
              (go :collect-token))
             ((#.+cs-whitespace+
               #.+cs-constituent+
               #.+cs-non-terminating-macro-character+
               #.+cs-terminating-macro-character+)
              (token-collect-character token y +ct-alphabetic+)
              (go :collect-multiple-escape-token)))))
       :parse-token
       ;; token can be of zero length...
       (return (values t token))))))




;; numeric-token ::= integer | ratio | float       
;; integer  ::= [sign] decimal-digit+ decimal-point 
;; integer  ::= [sign] digit+      
;; ratio    ::= [sign] {digit}+ slash {digit}+    
;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ exponent
;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ 
;; float    ::= [sign] {decimal-digit}+ exponent
;; float    ::= [sign] {decimal-digit}+ decimal-point {decimal-digit}* exponent
;; exponent ::=  exponent-marker [sign] {digit}+
;; 
;; consing-dot   ::= dot
;; 
;; symbol        ::= symbol-name
;;                 | package-marker symbol-name
;;                 | package-marker package-marker symbol-name
;;                 | package-name package-marker symbol-name
;;                 | package-name package-marker package-marker symbol-name
;; 
;; symbol-name   ::= {alphabetic}+ 
;; package-name  ::= {alphabetic}+ 



(defmacro defparser (name arguments &body body)
  "Defines a token parser function, which parses its argument token and returns
three values: a ok flag; a type of value; and a value parsed from the token.
When the ok flag is false, the type indicates whether it's a strong error,
and the value returned is an error message.
A strong error is a lexical error that is not ambiguous.  A weak error is
when the token could still be of another lexical category.
In the body of the parser, there are macrolet defined to REJECT or ACCEPT
the token, and to describe the parsed syntax with ALT, ZERO-OR-MORE, 
ONE-OR-MORE and OPT-SIGN."
  (let ((docu (extract-documentation body))
        (decl (extract-declarations body))
        (body (extract-body body)))
    `(defun ,name ,arguments
       ,@(when docu (list docu))
       ,@decl
       (macrolet ((reject (strongp &rest ctrlstring-and-args)
                    `(return-from ,',name
                       (values nil ,strongp
                               ,(when ctrlstring-and-args
                                      `(format nil ,@ctrlstring-and-args)))))
                  (accept (type token)
                    `(return-from ,',name (values t ,type ,token)))
                  (alt (&rest clauses)
                    `(cond ,@clauses))
                  (zero-or-more (test &body body)
                    `(loop :while ,test :do ,@body))
                  (one-or-more  (test &body body)
                    `(progn
                       (if ,test (progn ,@body) (reject nil))
                       (loop :while ,test :do ,@body)))
                  (opt-sign (sign token i)
                    `(alt ((>= ,i (token-length ,token)))
                          ((traitp +ct-plus-sign+  (token-char-traits ,token ,i))
                           (setf ,sign +1 ,i (1+ ,i)))
                          ((traitp +ct-minus-sign+ (token-char-traits ,token ,i))
                           (setf ,sign -1 ,i (1+ ,i))))))
         ,@body))))


(defparser parse-decimal-integer-token (token)
  "integer ::= [sign] decimal-digit+ decimal-point"
  (let ((sign 1)
        (mant 0)
        (i 0))
    (unless (< i (token-length token)) (reject nil))
    (unless (traitp +ct-decimal-point+
                    (token-char-traits token (1- (token-length token))))
      (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf mant (+ (* 10. mant) (digit-char-p (token-char token i)))
                       i (1+ i)))
    (if (and (= (1+ i) (token-length token))
             (traitp +ct-decimal-point+ (token-char-traits token i)))
        (accept 'integer (* sign mant))
        (reject t
                (if (= (1+ i) (token-length token))
                    "Missing decimal point in decimal integer ~S"
                    "Junk after decimal point in decimal integer ~S")
                (token-text token)))))


(defparser parse-integer-token (token)
  "integer ::= [sign] digit+"
  (let ((sign 1)
        (mant 0)
        (i 0))
    (unless (< i (token-length token)) (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i) *read-base*))
                 (setf mant (+ (* *read-base* mant)
                               (digit-char-p (token-char token i) *read-base*))
                       i (1+ i)))
    (if (= i (token-length token))
        (accept 'integer (* sign mant))
        (reject t "Junk after integer ~S" (token-text token)))))

(defun %parse-integer (string &key (start 0) (end nil) (radix 10.) (junk-allowed nil)
                       &aux (end (or end (length string))))
  (loop
     :named parse
     :with sign = (case (aref string start)
                    (#\+ (incf start) +1)
                    (#\- (incf start) -1)
                    (otherwise        +1))
     :with mant = 0
     :for i :from start :below end
     :do (let ((digit (digit-char-p (aref string i) radix)))
           (cond
             (digit         (setf mant (+ (* mant radix) digit)))
             (junk-allowed  (return-from parse (values (* sign mant) i)))
             (t             (error 'parse-error))) ())
     :finally (return-from parse (values (* sign mant) i))))


(defparser parse-ratio-token (token)
  "ratio ::= [sign] {digit}+ slash {digit}+"
  (let ((sign 1)
        (nume 0)
        (denu 0)
        (i 0))
    (unless (< i (token-length token)) (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i) *read-base*))
                 (setf nume (+ (* *read-base* nume)
                               (digit-char-p (token-char token i) *read-base*))
                       i (1+ i)))
    (if (traitp +ct-ratio-marker+ (token-char-traits token i))
        (incf i)
        (reject nil))
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i) *read-base*))
                 (setf denu (+ (* *read-base* denu)
                               (digit-char-p (token-char token i) *read-base*))
                       i (1+ i)))
    (cond
      ((< i (token-length token))
       (reject t "Junk after ratio ~S" (token-text token)))
      #+(or) ((zerop denu) (reject t "Zero denominator ratio ~S" (token-text token)))
      (t
       (accept 'ratio (/ (* sign nume) denu))))))


(defparser parse-float-1-token (token)
  "float ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent]
exponent ::=  exponent-marker [sign] {digit}+"
  (let ((sign 1)
        (nume 0)
        (denu 1)
        (type *READ-DEFAULT-FLOAT-FORMAT*)
        (esgn 1)
        (expo 0)
        (i 0))
    (opt-sign sign token i)
    (zero-or-more (and (< i (token-length token))
                       (traitp +ct-digit+ (token-char-traits token i))
                       (digit-char-p (token-char token i)))
                  (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                        i (1+ i)))
    (if (and (< i (token-length token))
             (traitp +ct-decimal-point+ (token-char-traits token i)))
        (incf i)
        (reject nil))
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                       denu (* 10. denu)
                       i (1+ i)))
    (when (and (< i (token-length token))
               (traitp +ct-float-exponent-marker+ (token-char-traits token i)))
      (cond
        ((traitp +ct-short-float-exponent-marker+ (token-char-traits token i))
         (setf type 'short-float))
        ((traitp +ct-single-float-exponent-marker+ (token-char-traits token i))
         (setf type 'single-float))
        ((traitp +ct-double-float-exponent-marker+ (token-char-traits token i))
         (setf type 'double-float))
        ((traitp +ct-long-float-exponent-marker+ (token-char-traits token i))
         (setf type 'long-float)))
      (incf i)
      (opt-sign esgn token i)
      (one-or-more (and (< i (token-length token))
                        (traitp +ct-digit+ (token-char-traits token i))
                        (digit-char-p (token-char token i)))
                   (setf expo (+ (* 10. expo) (digit-char-p (token-char token i)))
                         i (1+ i))))
    (if (= i (token-length token))
        (accept type
                (* (coerce (/ (* sign nume) denu) type)
                   (expt 10.0 (* esgn expo))))
        (reject t "Junk after floating point number ~S" (token-text token)))))


(defparser parse-float-2-token (token)
  "float ::= [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
exponent ::=  exponent-marker [sign] {digit}+"
  (let ((sign 1)
        (nume 0)
        (denu 1)
        (type *READ-DEFAULT-FLOAT-FORMAT*)
        (esgn 1)
        (expo 0)
        (i 0))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                       i (1+ i)))
    (when (and (< i (token-length token))
               (traitp +ct-decimal-point+ (token-char-traits token i)))
      (incf i)
      (one-or-more (and (< i (token-length token))
                        (traitp +ct-digit+ (token-char-traits token i))
                        (digit-char-p (token-char token i)))
                   (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                         denu (* 10. denu)
                         i (1+ i))))
    (unless (and (< i (token-length token))
                 (traitp +ct-float-exponent-marker+ (token-char-traits token i)))
      (reject nil))
    (cond
      ((traitp +ct-short-float-exponent-marker+ (token-char-traits token i))
       (setf type 'short-float))
      ((traitp +ct-single-float-exponent-marker+ (token-char-traits token i))
       (setf type 'single-float))
      ((traitp +ct-double-float-exponent-marker+ (token-char-traits token i))
       (setf type 'double-float))
      ((traitp +ct-long-float-exponent-marker+ (token-char-traits token i))
       (setf type 'long-float)))
    (incf i)
    (opt-sign esgn token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf expo (+ (* 10. expo) (digit-char-p (token-char token i)))
                       i (1+ i)))
    (if (= i (token-length token))
        (accept type
                (* (coerce (/ (* sign nume) denu) type)
                   (expt 10.0 (* esgn expo))))
        (reject t "Junk after floating point number ~S" (token-text token)))))


;; (defparser parse-consing-dot-token (token)
;;   "consing-dot ::= dot"
;;   (if (and (= 1 (token-length token))
;;            (traitp +ct-dot+ (token-char-traits token 0)))
;;       (accept 'consing-dot ".")
;;       (reject nil)))


(defparser parse-symbol-token (token)
  "symbol ::= symbol-name
symbol ::= package-marker symbol-name
symbol ::= package-marker package-marker symbol-name
symbol ::= package-name package-marker symbol-name
symbol ::= package-name package-marker package-marker symbol-name
symbol-name   ::= {alphabetic}+ 
package-name  ::= {alphabetic}+ "
  (let ((colon (position-if
                (lambda (traits) (traitp +ct-package-marker+ traits))
                (token-traits token))))
    (if colon
        (let* ((double-colon (and (< (1+ colon) (token-length token))
                                  (traitp +ct-package-marker+
                                          (token-char-traits token (1+ colon)))))
               (pname (subseq (token-text token) 0 colon))
               (sname (subseq (token-text token)
                              (+ colon (if double-colon 2 1)))))
          (when (position-if
                 (lambda (traits) (traitp +ct-package-marker+ traits))
                 (token-traits token) :start (+ colon (if double-colon 2 1)))
            (reject t "Too many package markers in token ~S" (token-text token)))
          (when (zerop colon)
            ;; Keywords always exist, so let's intern them before finding them.
            (setf pname "KEYWORD")
            (intern sname pname))
          ;; The following form thanks to Andrew Philpot <philpot@ISI.EDU>
          ;; corrects a bug when reading with double-colon uninterned symbols:
          (if (find-package pname)
              (if double-colon
                  (accept 'symbol (intern sname pname))
                  (multiple-value-bind (sym where) (find-symbol sname pname)
                    (if (eq where :external) 
                        (accept 'symbol sym)
                        (reject t "There is no external symbol named ~S in ~
                               the package named ~S" sname pname))))
              (reject t "There is no package with name ~S" pname)))
        ;; no colon in token, let's just intern the symbol in the current package:
        (accept 'symbol (intern (token-text token) *package*)))))


(defun parse-token (token)
  "
RETURN:  okp ; the parsed lisp object if okp, or an error message if (not okp)
"
  (let ((message nil))
    (macrolet
        ((rom (&body body)
           "Result Or Message"
           (if (null body)
               'nil
               (let ((vals (gensym)))
                 `(let ((,vals (multiple-value-list ,(car body))))
                    ;; (format *trace-output* "~S --> ~S~%" ',(car body) ,vals)
                    (if (first ,vals)
                        (values-list ,vals)
                        (progn
                          (when (second ,vals)
                            (setf message  (third ,vals)))
                          (rom ,@(cdr body)))))))))
      (multiple-value-bind (ok type object)
          (rom (parse-decimal-integer-token token)
               (parse-integer-token         token)
               (parse-ratio-token           token)
               (parse-float-1-token         token)
               (parse-float-2-token         token)
               ;; (parse-consing-dot-token     token)
               (parse-symbol-token          token))
        (declare (ignorable type))
        ;; (format *trace-output* "ok = ~S ; type = ~S ; object = ~S~%"
        ;;         ok type object)
        (values ok (if ok object message))))))



(defun all-dots-p (token)
  "
RETURN: Whether the token is all dots, (excluding escaped dots).
"
  (and (plusp (length (token-text token)))
       (every (lambda (traits) (traitp +ct-dot+ traits)) (token-traits token))))


(defun read-0/1 (input-stream eof-error-p eof-value recursive-p
                 preserve-whitespace-p first-char allowed-all-dots)
  "
DO:             Read zero or one token.  Use the *READTABLE*.

INPUT-STREAM:   The stream that is read.
EOF-ERROR-P:    Whether we should signal an END-OF-FILE error upon EOF.
EOF-VALUE:      Unless EOF-ERROR-P, the value to be returned in case of EOF.
RECURSIVE-P:    Whether the read is recursive.
                The *reference* table is reset only when RECURSIVE-P is false.
PRESERVE-WHITESPACE-P:
                Whether a terminating whitespace will be unread or not.
FIRST-CHAR:     NIL or a CHARACTER that is used first, before reading the stream.
                This should be faster than UNREAD-CHAR it, and foremost, it allows
                for two unread character, this FIRST-CHAR plus an actual UNREAD-CHAR one.
ALLOWED-ALL-DOTS:
                May be T in which case tokens containing only dots are allowed,
                or a (possibly empty) list of strings containing only dots that are
                explicitely allowed (others rejected). Typically (\".\").

RETURN:         tokenp == t    ; an unparsed (alldots) token.  Or
                tokenp == :EOF ; the eof-value.  Or
                tokenp == NIL  ; a list of values read.
"
  (multiple-value-bind (tokenp token)
      (read-token input-stream eof-error-p eof-value recursive-p
                  preserve-whitespace-p first-char *readtable*)
    (if (eq 't tokenp)
        (cond
          (*read-suppress*
           (values nil (list nil)))
          ((or (eq 't allowed-all-dots)
               (not (all-dots-p token))) ; We got a token, let's parse it.
           (values nil (list
                        (multiple-value-bind (okp object)
                            (funcall (readtable-parse-token *readtable*) token)
                          (if okp
                              object
                              (serror 'simple-reader-error input-stream
                                      "~A" object))))))
          ((member (token-text token) allowed-all-dots :test (function string=))
           (values t token))
          (t
           (serror 'simple-reader-error input-stream
                   "a token consisting only of dots cannot be ~
                   meaningfully read in")))
        (values tokenp token)))) 




(defun read-1 (input-stream eof-error-p eof-value
               recursive-p preserve-whitespace-p first-char allowed-all-dots)
   "
DO:             Read exactly one token.  Use the *READTABLE*.

INPUT-STREAM:   The stream that is read.
EOF-ERROR-P:    Whether we should signal an END-OF-FILE error upon EOF.
EOF-VALUE:      Unless EOF-ERROR-P, the value to be returned in case of EOF.
RECURSIVE-P:    Whether the read is recursive.
                The *reference* table is reset only when RECURSIVE-P is false.
PRESERVE-WHITESPACE-P:
                Whether a terminating whitespace will be unread or not.
FIRST-CHAR:     NIL or a CHARACTER that is used first, before reading the stream.
                This should be faster than UNREAD-CHAR it, and foremost, it allows
                for two unread character, this FIRST-CHAR plus an actual UNREAD-CHAR one.
ALLOWED-ALL-DOTS:
                May be T in which case tokens containing only dots are allowed,
                or a (possibly empty) list of strings containing only dots that are
                explicitely allowed (others rejected). Typically (\".\").

RETURN:         The token read, or
                when EOF-ERROR-P is false and EOF occurs, EOF-VALUE.
" (loop
     :for (tokenp token) = (multiple-value-list
                            (read-0/1 input-stream eof-error-p eof-value
                                      recursive-p preserve-whitespace-p
                                      first-char allowed-all-dots))
     :until (or (eq :eof tokenp) token)
     :finally (return (if (eq :eof tokenp)
                          token
                          (first token)))))


(defun read (&optional input-stream
             (eof-error-p t) (eof-value nil)
             (recursive-p nil))
  "URL: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm"
  (read-1 input-stream eof-error-p eof-value recursive-p  nil  nil '()))

      
(defun read-preserving-whitespace (&optional input-stream
                                   (eof-error-p t) (eof-value nil)
                                   (recursive-p nil))
  "URL: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm"
  (read-1 input-stream eof-error-p eof-value recursive-p  t    nil '()))


(defun read-delimited-list (char &optional (input-stream *standard-input*)
                            (recursive-p nil))
  "URL: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_del.htm"
  (loop
     :with result = '()
     :for peek = (peek-char t input-stream nil input-stream recursive-p)
     :do (cond
           ((eql peek input-stream)
            (serror 'simple-end-of-file input-stream
                    "input stream ~S has reached its end" input-stream))
           ((char= peek char)
            (read-char input-stream nil nil recursive-p)
            (return-from read-delimited-list (nreverse result)))
           (t
            (multiple-value-bind (kind tokens)
                (read-0/1 input-stream t nil recursive-p nil nil '())
              (declare (ignore kind))
              (when tokens
                (push (first tokens) result)))))))


(defun read-from-string (string &optional (eof-error-p t) (eof-value nil)
                         &key (start 0) (end nil) (preserve-whitespace nil))
  (let ((index 0))
    (values
     (with-input-from-string (input string :index index :start start :end end)
       (funcall (if preserve-whitespace
                    (function read-preserving-whitespace)
                    (function read))
                input eof-error-p eof-value))
     index)))


(defun readtable-case (readtable)
  (slot-value readtable 'case))

(defun (setf readtable-case) (value readtable)
  (check-type value (member :upcase :downcase :preserve :invert))
  (setf (slot-value readtable 'case) value))


(defun readtablep (object) (typep object 'readtable))


(defun make-dispatch-macro-character
    (char &optional (non-terminating-p nil) (readtable *readtable*))
  (let ((rst  (readtable-syntax-table readtable)))
    (setf (character-description rst char)
          (make-instance 'character-description
            :syntax (if non-terminating-p
                        +cs-non-terminating-macro-character+
                        +cs-terminating-macro-character+)
            :traits (character-constituent-traits
                     (character-description rst char))
            :macro (function reader-macro-dispatch-function)
            :dispatch (make-hash-table)))))

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (readtable *readtable*))
  (let* ((rst  (readtable-syntax-table readtable))
         (cd   (character-description rst disp-char)))
    (unless (character-dispatch cd)
      (error "~S is not  a dispatch macro character" disp-char))
    (and (character-dispatch cd)
         (gethash (char-upcase sub-char) (character-dispatch cd)))))

(defun set-dispatch-macro-character (disp-char sub-char new-function
                                     &optional (readtable *readtable*))
  (let* ((rst  (readtable-syntax-table readtable))
         (cd   (character-description rst disp-char)))
    (unless (character-dispatch cd)
      (error "~S is not  a dispatch macro character" disp-char))
    (setf (gethash (char-upcase sub-char) (character-dispatch cd))
          new-function))
  t)


(defun get-macro-character (char &optional (readtable *readtable*))
  (let* ((rst  (readtable-syntax-table readtable))
         (cd   (character-description rst char)))
    (values (character-macro cd)
            (= (character-syntax cd) +cs-non-terminating-macro-character+))))

(defun set-macro-character (char new-function &optional (non-terminating-p nil)
                            (readtable *readtable*))
  (let* ((rst  (readtable-syntax-table readtable)))
    (setf (character-description rst char)
          (make-instance 'character-description
            :syntax (if non-terminating-p
                        +cs-non-terminating-macro-character+
                        +cs-terminating-macro-character+)
            :traits (character-constituent-traits
                     (character-description rst char))
            :macro new-function)))
  t)


(defun set-indirect-dispatch-macro-character (disp-char sub-char function-name
                                              &optional (readtable *readtable*))
  "Like set-dispatch-macro-character, but with an indirect function, 
to enable TRACE and redefinitions of the dispatch macro character function."
  (set-dispatch-macro-character
   disp-char sub-char
   (compile nil
            (let ((s (gensym)) (c (gensym)) (a (gensym)))
              `(lambda (,s ,c ,a) (,function-name ,s ,c ,a))))
   readtable))

(defun set-indirect-macro-character (char function-name
                                     &optional (readtable *readtable*))
  "Like set-macro-character, but with an indirect function, 
to enable TRACE and redefinitions of the macro character function."
  (set-macro-character
   char
   (compile nil
            (let ((s (gensym)) (a (gensym)))
              `(lambda (,s ,a) (,function-name ,s ,a))))
   readtable))



;; Copied from com.informatimago.common-lisp.utility to avoid package use loop.
(defun copy-hash-table (table)
  "
TABLE:  (OR NULL HASH-TABLE)
RETURN: If TABLE is NIL, then NIL, 
        else a new HASH-TABLE with the same TEST, SIZE, REHASH-THRESHOLD 
        REHASH-SIZE and KEY->VALUE associations than TABLE.
        (Neither the keys nor the values are copied).
"
  (check-type table (or null hash-table))
  (when table
    (let ((copy (make-hash-table
                 :test             (hash-table-test             table)
                 :size             (hash-table-size             table)
                 :rehash-threshold (hash-table-rehash-threshold table)
                 :rehash-size      (hash-table-rehash-size      table))))
      (maphash (lambda (k v) (setf (gethash k copy) v)) table)
      copy)))


(defun set-syntax-from-char (to-char from-char
                             &optional (to-readtable *readtable*)
                             (from-readtable *standard-readtable*))
  (let* ((frst  (readtable-syntax-table from-readtable))
         (trst  (readtable-syntax-table   to-readtable))
         (fcd   (character-description frst from-char))
         (tcd   (character-description trst   to-char)))
    (setf (slot-value tcd 'syntax)
          (make-instance 'character-description
            :syntax (character-syntax fcd)
            :traits (character-constituent-traits fcd)
            :macro (character-macro fcd)
            :dispatch (copy-hash-table (character-dispatch fcd)))))
  t)


;;;----------------------------------------
;;; STANDARD READER MACRO FUNCTIONS
;;;----------------------------------------

(defun reader-macro-line-comment (stream ch)
  "Standard ; macro reader."
  (declare (ignore ch))
  (read-line stream nil)
  (values))

(defun reader-macro-string (stream delim)
  "Standard \" macro reader."
  (flet ((error-eof ()
           (serror 'simple-end-of-file stream
                    "input stream ~S ends within a string" stream)))
    (loop
       :with rst    = (readtable-syntax-table *readtable*)
       :with string = (make-array 64 :element-type 'character
                                  :adjustable t :fill-pointer 0)
       :for ch      = (read-char stream nil nil t)
       :do (cond
             ((null ch)
              (error-eof))
             ((eql ch delim)
              (return-from reader-macro-string (copy-seq string)))
             ((= (character-syntax (character-description rst ch))
                 +cs-single-escape+)
              (let ((next (read-char stream nil nil)))
                (when (null next)
                  (error-eof))
                (vector-push-extend next string)))
             (t (vector-push-extend ch   string))))))


(defun reader-macro-quote (stream ch)
  "Standard ' macro reader."
  (declare (ignore ch))
  `(quote ,(read stream t nil t)))


(defun reader-macro-backquote (stream ch)
  "Standard ` macro reader."
  (declare (ignore ch))
  `(backquote ,(read stream t nil t)))


(defun reader-macro-comma (stream ch)
  "Standard , macro reader."
  (declare (ignore ch))
  `(,(if (char= #\@ (peek-char nil stream t nil t)) 'splice 'unquote)
     ,(read stream t nil t)))


(defun reader-macro-left-parenthesis (stream ch)
  "Standard ( macro reader."
  (declare (ignore ch))
  (loop
     :with result     = (cons nil nil)
     :with last-cons  = result
     :with last-cdr-p = nil
     :for ch = (progn (peek-char t stream nil t) (read-char stream t nil t))
     ;; :do (print `(:result ,result :last-cons ,last-cons
     ;;                      :last-cdr-p ,last-cdr-p :ch ,ch))
     :do (flet ((read-and-nconc (ch)
                  (let ((objects
                         (nth-value 1 (read-0/1 stream t nil t nil ch '()))))
                    (when objects
                      (case last-cdr-p
                        ((nil)     (setf (cdr last-cons) objects
                                         ;; (list (first objects))
                                         last-cons       (cdr last-cons)))
                        ((t)       (setf (cdr last-cons) (first objects)
                                         last-cdr-p      :done))
                        (otherwise (serror 'simple-reader-error stream
                                           "illegal end of dotted list")))))))
           (cond
             ((char= #\) ch) (loop-finish))
             ((char= #\. ch)
              (if (token-delimiter-p (peek-char nil stream t nil t))
                  (if (eq result last-cons)
                      (serror 'simple-reader-error stream
                              "missing an object before the \".\" in a cons cell")
                      (case last-cdr-p
                        ((nil)     (setf last-cdr-p t))
                        ((t)       (serror 'simple-reader-error stream
                                           "token \".\" not allowed here"))
                        (otherwise (serror 'simple-reader-error stream
                                           "illegal end of dotted list"))))
                  (read-and-nconc ch)))
             (t
              (read-and-nconc ch))))
     
     :finally (if (eq last-cdr-p 't)
                  (serror 'simple-reader-error stream
                                         "illegal end of dotted list")
                  (return (cdr result)))))


(defun reader-macro-error-start (stream ch)
  (serror 'simple-reader-error stream
          "an object cannot start with ~C" ch))

;;;----------------------------------------
;;; STANDARD READER DISPATCH MACRO FUNCTIONS
;;;----------------------------------------

(defun reader-dispatch-macro-label-reference   (stream arg sub-char)
  "Standard ## dispatch macro reader."
  (declare (ignore sub-char))
  (when (null arg)
    (serror 'simple-reader-error stream
            "a number must be given between # and #"))
  (multiple-value-bind (object presentp) (gethash arg *references*)
    (if presentp
        object
        (serror 'simple-reader-error stream "undefined label #~D#" arg))))


(defun reader-dispatch-macro-label-definition  (stream arg sub-char)
  "Standard #= dispatch macro reader."
  (declare (ignore sub-char))
  (when (null arg)
    (serror 'simple-reader-error stream
            "a number must be given between # and ="))
  (multiple-value-bind (object presentp) (gethash arg *references*)
    (if presentp
        (serror 'simple-reader-error stream
                "label #~D=~S already defined as ~S"
                (read stream t nil t) arg object)
        (setf (gethash arg *references*) (read stream t nil t)))))


(defun eval-feature (expression stream)
  "Evaluates a feature expression as a BOOLEAN."
  (flet ((illegal-feature ()
           (serror 'simple-reader-error stream "illegal feature ~S" expression))
         (eval-term (term)
           (eval-feature term stream)))
    (cond
      ;; Some implementations accept any atom:
      ((atom    expression) (not (null (member expression *features*))))
      (t (case (first expression)
           ((:not) (if (cddr expression)
                       (illegal-feature)
                       (not (eval-feature (second expression) stream))))
           ((:and) (every (function eval-term) (rest expression)))
           ((:or)  (some  (function eval-term) (rest expression)))
           (t      (illegal-feature)))))))


(defun read-feature (stream affirmativep)
  "Reads a feature expression, and possibly eats one following sexp"
  (let ((expression (let ((*package*  (find-package "KEYWORD"))
                          (*read-suppress* nil))
                      (read stream nil stream t))))
    ;; (print `(:read-feature ,expression))
    (when (eq expression stream)
      (serror 'simple-end-of-file stream
              "EOF in ~S while reading the feature expression" stream))
    (unless (funcall (if affirmativep
                         (function identity)
                         (function not))
                     (eval-feature expression stream))
      ;; (print `(:read-feature ,expression false we eat))
      (let ((*read-suppress* t))
        ;; (print `(:read-feature ,(read stream t nil nil) :eaten))
        (read stream t nil nil)))
    (values)))


(defun reader-dispatch-macro-feature           (stream arg sub-char)
  "Standard #+ dispatch macro reader."
  (declare (ignore sub-char arg))
  (read-feature stream t))


(defun reader-dispatch-macro-not-feature       (stream arg sub-char)
  "Standard #- dispatch macro reader."
  (declare (ignore sub-char arg))
  (read-feature stream nil))


;; (defparameter *rt*
;;   (let ((rt (copy-readtable)))
;;     (set-dispatch-macro-character
;;      #\# #\+ (function reader-dispatch-macro-feature) rt)
;;     (set-dispatch-macro-character
;;      #\# #\- (function reader-dispatch-macro-not-feature) rt)
;;     rt))


(defun reader-dispatch-macro-read-eval         (stream arg sub-char)
  "Standard #. dispatch macro reader."
  (declare (ignore sub-char arg))
  (if *read-eval*
      (eval (read stream t nil t))
      (serror 'simple-reader-error stream
              "*READ-EVAL* = NIL does not allow the evaluation of ~S"
              (read stream t nil t))))


(defun reader-dispatch-macro-uninterned        (stream arg sub-char)
  "Standard #: dispatch macro reader."
  (declare (ignore sub-char arg))
  (multiple-value-bind (tokenp token)
      (read-token stream t nil t nil nil *readtable*)
    (if tokenp
        (make-symbol (token-text token))
        (serror 'simple-reader-error stream
                "token expected after #:"))))


(defun reader-dispatch-macro-unreadable        (stream arg sub-char)
  "Standard #< dispatch macro reader."
  (declare (ignore sub-char arg))
  (serror 'simple-reader-error stream
          "objects printed as #<...> cannot be read back in"))


(defun reader-dispatch-macro-COMMENT           (stream arg sub-char)
  "Standard #| dispatch macro reader."
  (declare (ignore sub-char arg))
  ;; #|...|# is treated as a comment by the reader. It must be balanced
  ;; with respect to other occurrences of #| and |#, but otherwise may
  ;; contain any characters whatsoever.
  (loop
     :with level = 1
     :with state = :normal
     :until (zerop level)
     :do (case state
           ((:normal) (case (read-char stream t nil t)
                        ((#\#)              (setf state :sharp))
                        ((#\|)              (setf state :pipe))))
           ((:sharp)  (case (read-char stream t nil t)
                        ((#\#))
                        ((#\|) (incf level) (setf state :normal))
                        (otherwise          (setf state :normal))))
           ((:pipe)   (case (read-char stream t nil t)
                        ((#\#) (decf level) (setf state :normal))
                        ((#\|))
                        (otherwise          (setf state :normal))))))
  (values))


(defun reader-dispatch-macro-function          (stream arg sub-char)
  "Standard #' dispatch macro reader."
  (declare (ignore sub-char arg))
  `(cl:function ,(read stream t nil t)))


(defun reader-dispatch-macro-vector            (stream arg sub-char)
  "Standard #( dispatch macro reader."
  (declare (ignore sub-char))
  ;; If an unsigned decimal integer appears between the # and (, it
  ;; specifies explicitly the length of the vector. The consequences are
  ;; undefined if the number of objects specified before the closing )
  ;; exceeds the unsigned decimal integer. If the number of  objects
  ;; supplied before the closing ) is less than the unsigned decimal
  ;; integer but greater than zero, the last object is used to fill all
  ;; remaining elements of the  vector. The consequences are undefined if
  ;; the unsigned decimal integer is non-zero and number of objects
  ;; supplied before the closing ) is zero.  In that case, we let the
  ;; implementation initialize the vector.
  ;;
  ;; Thanks to Budden for having signaled a bug in the first version of this function,
  ;; and thanks to Yulian Tarantuk for signaling the "comment before closing parenthesis" bug.
  (flet ((finish-vector (vector i)
           (if arg
               (progn
                 (cond
                   ((>= i arg)
                    ;; vector is longer than the explicitly given length
                    ;; We just eat the remaining stuff.
                    (loop
                       :until (char= #\) (peek-char t stream t nil t))
                       :do (let ((*read-suppress* t))
                         (read-0/1 stream t nil t nil nil '()))
                       :finally (read-char stream nil nil t)))
                   ;; vector is shorter.
                   ((plusp i)
                    ;; If we have at least one element in,
                    ;; we replicate it till the end. 
                    (loop
                       :with last-item = (aref vector (1- i))
                       :for j :from i :below arg
                       :do (setf (aref vector j) last-item)))
                   ;; Otherwise we will let it up to the implementation
                   ;; to do its implementation dependent thing.
                   )
                 vector)
               (copy-seq vector))))
    (loop
       :with vector = (if arg
                          (make-array arg)
                          (make-array 1024 :adjustable t :fill-pointer 0))
       :for i :from 0 :while (or (not arg) (< i arg))
       :do (let ((peek (peek-char t stream nil stream t)))
             (cond
              ((eql peek stream)
               (serror 'simple-end-of-file stream
                       "input stream ~S has reached its end" stream))
              ((char= peek #\))
               (read-char stream nil nil t)
               (return-from reader-dispatch-macro-vector (finish-vector vector i)))
              (t
               (multiple-value-bind (kind tokens)
                   (read-0/1 stream t nil t nil nil '())
                 (declare (ignore kind)) ; always nil here.
                 (when tokens
                   (if arg
                       (setf (aref vector i) (first tokens))
                       (vector-push-extend (first tokens) vector)))))))
       :finally (return-from reader-dispatch-macro-vector (finish-vector vector i)))))





(defun reader-dispatch-macro-bit-vector        (stream arg sub-char)
  "Standard #* dispatch macro reader.
URL: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhd.htm
"
  (declare (ignore sub-char))
  ;; Syntax: #*<<bits>>
  ;; 
  ;; A simple bit vector is constructed containing the indicated bits (0's
  ;; and 1's), where the leftmost bit has index zero and the subsequent
  ;; bits have increasing indices.
  ;; 
  ;; Syntax: #<<n>>*<<bits>>
  ;; 
  ;; With an argument n, the vector to be created is of length n. If the
  ;; number of bits is less than n but greater than zero, the last bit is
  ;; used to fill all remaining bits of the bit vector.
  ;; 
  ;; The notations #* and #0* each denote an empty bit vector.
  ;; 
  ;; Regardless of whether the optional numeric argument n is provided, the
  ;; token that follows the asterisk is delimited by a normal token
  ;; delimiter. However, (unless the  value of *read-suppress* is true) an
  ;; error of type reader-error is signaled if that  token is not composed
  ;; entirely of 0's and 1's, or if n was supplied and the token is
  ;; composed of more than n bits, or if n is greater than one, but no bits
  ;; were specified.  Neither a single escape nor a multiple escape is
  ;; permitted in this token.
  (if arg
      (loop
         :with vector = (make-array arg :element-type 'bit :initial-element 0)
         :for i :from 0 :below arg
         :while (let ((ch (peek-char nil stream nil nil t)))
                  (and ch (not (token-delimiter-p ch))))
         :do (setf (aref vector i) (digit-char-p (read-char stream nil nil t)))
         :finally (progn
                    (cond
                      ((>= i arg)
                       (let ((*read-suppress* t))
                         (loop
                            :while (let ((ch (peek-char nil stream nil nil t)))
                                     (and ch (not (token-delimiter-p ch))))
                            :do (read-char stream nil nil t))))
                      ((plusp (aref vector (1- i)))
                       (loop
                          :for j :from i :below arg
                          :do (setf (aref vector j) 1))))
                    (return vector)))
      (loop
         :with vector = (make-array 1024 :adjustable t :fill-pointer 0
                                    :element-type 'bit :initial-element 0)
         :while (let ((ch (peek-char nil stream nil nil t)))
                  (and ch (not (token-delimiter-p ch))))
         ;; TODO: Check the behavior when the character is not a bit.
         :do (vector-push-extend (digit-char-p (read-char stream nil nil t)) vector)
         :finally (return (copy-seq vector)))))


(defun reader-dispatch-macro-CHAR              (stream arg sub-char)
  "Standard #\\ dispatch macro reader."
  (declare (ignore sub-char arg))
  (read-char stream t nil t))


(defun reader-dispatch-macro-ARRAY             (stream arg sub-char)
  "Standard #A dispatch macro reader."
  (declare (ignore sub-char))
  (let ((initial-contents (read stream t nil t)))
    (labels ((collect-dimensions (n contents dimensions)
             (if (zerop n)
                 (nreverse dimensions)
                 (collect-dimensions (1- n) (first contents)
                                     (cons (length contents) dimensions)))))
      ;; TODO: we rely on make-array to raise some errors that it may not raise...
      (make-array (collect-dimensions (or arg 1) initial-contents '())
                  :initial-contents initial-contents))))



(defun read-rational-in-base (stream arg sub-char *read-base*)
  "
DO:      Read a rational number in the base specified.
RETURN:  The rational read.
"
  (when arg (serror stream "no number allowed between # and ~A" sub-char))
  (let ((value (read stream t nil t)))
    (if (rationalp value)
        value
        (serror stream
                "token \"~A\" after #~A is not a rational number in base ~D"
                sub-char *read-base*))))

(defun reader-dispatch-macro-BINARY            (stream arg sub-char)
  "Standard #B dispatch macro reader."
  (read-rational-in-base stream arg sub-char 2.))

(defun reader-dispatch-macro-OCTAL             (stream arg sub-char)
  "Standard #O dispatch macro reader."
  (read-rational-in-base stream arg sub-char 8.))

(defun reader-dispatch-macro-HEXADECIMAL       (stream arg sub-char)
  "Standard #X dispatch macro reader."
  (read-rational-in-base stream arg sub-char 16.))

(defun reader-dispatch-macro-RADIX             (stream arg sub-char)
  "Standard #R dispatch macro reader."
  (unless arg
    (serror stream "the number base must be given between # and ~A" sub-char)) 
  (read-rational-in-base stream nil sub-char arg))


;; Copied from com.informatimago.common-lisp.list to avoid package use loop.
(defun proper-list-p (object)
  "
RETURN: whether object is a proper list
NOTE:   terminates with any kind of list, dotted, circular, etc.
"
  (labels ((proper (current slow)
             (cond ((null current)       t)
                   ((atom current)       nil)
                   ((null (cdr current)) t)
                   ((atom (cdr current)) nil)
                   ((eq current slow)    nil)
                   (t                    (proper (cddr current) (cdr slow))))))
    (and (listp object) (proper object (cons nil object)))))


(defun reader-dispatch-macro-COMPLEX           (stream arg sub-char)
  "Standard #C dispatch macro reader."
  (declare (ignore sub-char arg))
  (let ((c (read stream t nil t)))
    (unless (and (proper-list-p c)
                 (= 2 (length c))
                 (every (function realp) c))
      (serror 'simple-reader-error stream
              "bad syntax for complex number: #C~S" c))
    (complex (first c) (second c))))


(defun reader-dispatch-macro-PATHNAME          (stream arg sub-char)
  "Standard #P dispatch macro reader."
  (declare (ignore sub-char arg))
  (pathname (read stream t nil t)))


(defun reader-dispatch-macro-STRUCTURE         (stream arg sub-char)
  "Standard #S dispatch macro reader."
  (declare (ignore sub-char arg))
  (let* ((data (read stream t nil t))
         (constructor (intern (format nil "MAKE-~A" (first data))))
         (arguments   (loop
                         :with keyword-package = (find-package "KEYWORD")
                         :for (k v) :on (rest data) :by (function cddr)
                         :collect (intern (string k) keyword-package)
                         :collect v)))
    (apply constructor arguments)))


;;;;
;;;;



(defun test-proper-list-p ()
  (assert
   (every 
    (function identity)
    (mapcar (lambda (test) (eq (first test) (proper-list-p (second test))))
            '((nil x)
              (t ())
              (t (a))
              (t (a b))
              (t (a b c))
              (t (a b c d))
              (nil (a . x))
              (nil (a b . x))
              (nil (a b c . x))
              (nil (a b c d . x))
              (nil #1=(a . #1#))
              (nil #2=(a b . #2#))
              (nil #3=(a b c . #3#))
              (nil #4=(a b c d . #4#))
              (nil (1 . #1#))
              (nil (1 2 . #1#))
              (nil (1 2 3 . #1#))
              (nil (1 2 3 4 . #1#))
              (nil (1 . #2#))
              (nil (1 2 . #2#))
              (nil (1 2 3 . #2#))
              (nil (1 2 3 4 . #2#))
              (nil (1 . #3#))
              (nil (1 2 . #3#))
              (nil (1 2 3 . #3#))
              (nil (1 2 3 4 . #3#))
              (nil (1 . #4#))
              (nil (1 2 . #4#))
              (nil (1 2 3 . #4#))
              (nil (1 2 3 4 . #4#)))))))
;;;;









(defmethod initialize-instance
    :after ((self readtable) &rest rest &key &allow-other-keys)
  (unless (getf rest :syntax-table)
    (macrolet ((smc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-macro-character
                                  ,(first clause)
                                  (function ,(second clause))
                                  ,(third clause)
                                  self))
                              clauses))))
      (smc
       (#\; reader-macro-line-comment     nil)
       (#\" reader-macro-string           nil)
       (#\' reader-macro-quote            nil)
       (#\` reader-macro-backquote        nil)
       (#\, reader-macro-comma            nil)
       (#\( reader-macro-left-parenthesis nil)
       (#\) reader-macro-error-start      nil)))
    (macrolet ((dmc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-dispatch-macro-character
                                  ,(first  clause)
                                  ,(second clause)
                                  (function ,(third clause))
                                  self))
                              clauses))))
      (make-dispatch-macro-character #\# t self)
      (dmc
       (#\# #\SPACE   READER-DISPATCH-MACRO-ERROR-INVALID)
       (#\# #\NEWLINE READER-DISPATCH-MACRO-ERROR-INVALID)
       (#\# #\# READER-DISPATCH-MACRO-LABEL-REFERENCE)
       (#\# #\' READER-DISPATCH-MACRO-FUNCTION)
       (#\# #\( READER-DISPATCH-MACRO-VECTOR)
       (#\# #\* READER-DISPATCH-MACRO-BIT-VECTOR)
       (#\# #\+ READER-DISPATCH-MACRO-FEATURE)
       (#\# #\- READER-DISPATCH-MACRO-NOT-FEATURE)
       (#\# #\. READER-DISPATCH-MACRO-READ-EVAL)
       (#\# #\: READER-DISPATCH-MACRO-UNINTERNED)
       (#\# #\< READER-DISPATCH-MACRO-UNREADABLE)
       (#\# #\= READER-DISPATCH-MACRO-LABEL-DEFINITION)
       (#\# #\A READER-DISPATCH-MACRO-ARRAY)
       (#\# #\B READER-DISPATCH-MACRO-BINARY)
       (#\# #\C READER-DISPATCH-MACRO-COMPLEX)
       (#\# #\O READER-DISPATCH-MACRO-OCTAL)
       (#\# #\P READER-DISPATCH-MACRO-PATHNAME)
       (#\# #\R READER-DISPATCH-MACRO-RADIX)
       (#\# #\S READER-DISPATCH-MACRO-STRUCTURE)
       (#\# #\X READER-DISPATCH-MACRO-HEXADECIMAL)
       (#\# #\\ READER-DISPATCH-MACRO-CHAR)
       (#\# #\| READER-DISPATCH-MACRO-COMMENT)
       ;; clisp extensions:
       ;; (#\# #\! reader-dispatch-macro-executable)
       ;; (#\# #\" reader-dispatch-macro-clisp-pathname)
       ;; (#\# #\, reader-dispatch-macro-load-eval)
       ;; (#\# #\Y SYSTEM::CLOSURE-READER)
       ))))


(setf *standard-readtable* (copy-readtable nil)
      *readtable*          (copy-readtable nil))



;; or could go to UTILITIES, but this version will run on our own readtables...
(defun list-all-macro-characters (&optional (*readtable* *readtable*))
  "
RETURN: A list of all the macro and dispatch-macro characters in the readtable.
"
  (loop
     :with results = '()
     :for code :from 0 :below CHAR-CODE-LIMIT
     :for ch = (code-char code)
     :do (multiple-value-bind (fun ntp) (get-macro-character ch)
           (when (or fun ntp)
             (push (list ch fun ntp
                         (when (handler-case
                                   (progn (get-dispatch-macro-character ch #\a)
                                          t)
                                 (error () nil))
                           (loop
                              :for code :from 0 :below char-code-limit
                              :for sub = (code-char code)
                              :for fun = (get-dispatch-macro-character ch sub)
                              :when fun
                              :collect (list sub fun)))) results)))
     :finally (return results)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-extract ()
  (let ((test-cases
         '(()
           ("Result")
           ("Doc" "Result")
           ((declare (ignore it)))
           ((declare (ignore it)) "Result")
           ((declare (ignore it)) "Doc" "Result")
           ((declare (ignore it)) (declare (ignore it)))
           ((declare (ignore it)) (declare (ignore it)) "Result")
           ((declare (ignore it)) "Doc" (declare (ignore it)))
           ((declare (ignore it)) (declare (ignore it)) "Doc" "Result")
           ((declare (ignore it)) "Doc" (declare (ignore it)) "Result")
           ((declare (ignore it)) "Doc" "Illegal" (declare (ignore it)) "Result")
           ("Doc" (declare (ignore it)) "Result")
           ("Doc" (declare (ignore it)) (declare (ignore it)))
           ("Doc" (declare (ignore it)) (declare (ignore it)) "Result")
           ("Doc" (declare (ignore it)) "Illegal" (declare (ignore it)) "Result")
           )))
    (assert
     (equalp '(NIL NIL "Doc" NIL NIL "Doc" NIL NIL "Doc"
               "Doc" "Doc" "Doc" "Doc" "Doc" "Doc" "Doc" )
             (mapcar (function extract-documentation) test-cases)))
    (assert
     (equalp '(nil nil nil
               ((declare (ignore it)))
               ((declare (ignore it)))
               ((declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it))) ;; "Illegal"
               ((declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it)) (declare (ignore it)))
               ((declare (ignore it)))) ;; "Illegal"
             (mapcar (function extract-declarations) test-cases)))
    (assert
     (equalp '((NIL) ("Result") ("Result")
               ((DECLARE (IGNORE IT))) ("Result") ("Result")
               ((DECLARE (IGNORE IT))) ("Result") ((DECLARE (IGNORE IT)))
               ("Result") ("Result") ("Illegal" (DECLARE (IGNORE IT)) "Result")
               ("Result") ((DECLARE (IGNORE IT))) ("Result")
               ("Illegal" (DECLARE (IGNORE IT)) "Result"))
             (mapcar (function extract-body) test-cases)))
    :success))


(defun test-reader ()
  (let ((*read-base* 10)
        (*read-eval* t)
        (*read-suppress* nil)
        (*READ-DEFAULT-FLOAT-FORMAT* 'single-float))
    (dolist (test
              '(
                ;; integer  ::= [sign] digit+      
                (nil "0"  0.)
                (nil "1"  1.)
                (nil "2"  2.)
                (nil "9"  9.)
                (nil "10" 10.)
                (nil "11" 11.)
                (nil "12" 12.)
                (nil "19" 19.)
                (((*read-base* 3.)) "0"  0.)
                (((*read-base* 3.)) "1"  1.)
                (((*read-base* 3.)) "2"  2.)
                (((*read-base* 3.)) "9"  |9|)
                (((*read-base* 3.)) "10" 3.)
                (((*read-base* 3.)) "11" 4.)
                (((*read-base* 3.)) "13" |13|)
                (nil "-0"  -0.)
                (nil "-1"  -1.)
                (nil "-2"  -2.)
                (nil "-9"  -9.)
                (nil "-10" -10.)
                (nil "-11" -11.)
                (nil "-12" -12.)
                (nil "-19" -19.)
                (((*read-base* 3.)) "-0"  -0.)
                (((*read-base* 3.)) "-1"  -1.)
                (((*read-base* 3.)) "-2"  -2.)
                (((*read-base* 3.)) "-9"  |-9|)
                (((*read-base* 3.)) "-10" -3.)
                (((*read-base* 3.)) "-11" -4.)
                (((*read-base* 3.)) "-13" |-13|)
                (nil "+0"  +0.)
                (nil "+1"  +1.)
                (nil "+2"  +2.)
                (nil "+9"  +9.)
                (nil "+10" +10.)
                (nil "+11" +11.)
                (nil "+12" +12.)
                (nil "+19" +19.)
                (((*read-base* 3.)) "+0"  +0.)
                (((*read-base* 3.)) "+1"  +1.)
                (((*read-base* 3.)) "+2"  +2.)
                (((*read-base* 3.)) "+9"  |+9|)
                (((*read-base* 3.)) "+10" +3.)
                (((*read-base* 3.)) "+11" +4.)
                (((*read-base* 3.)) "+13" |+13|)
                ;; integer  ::= [sign] decimal-digit+ decimal-point 
                (nil "0."  0.)
                (nil "1."  1.)
                (nil "2."  2.)
                (nil "9."  9.)
                (nil "10." 10.)
                (nil "11." 11.)
                (nil "12." 12.)
                (nil "19." 19.)
                (((*read-base* 3.)) "0."  0.)
                (((*read-base* 3.)) "1."  1.)
                (((*read-base* 3.)) "2."  2.)
                (((*read-base* 3.)) "9."  9.)
                (((*read-base* 3.)) "10." 10.)
                (((*read-base* 3.)) "11." 11.)
                (((*read-base* 3.)) "13." 13.)
                (nil "-0."  -0.)
                (nil "-1."  -1.)
                (nil "-2."  -2.)
                (nil "-9."  -9.)
                (nil "-10." -10.)
                (nil "-11." -11.)
                (nil "-12." -12.)
                (nil "-19." -19.)
                (((*read-base* 3.)) "-0."  -0.)
                (((*read-base* 3.)) "-1."  -1.)
                (((*read-base* 3.)) "-2."  -2.)
                (((*read-base* 3.)) "-9."  -9.)
                (((*read-base* 3.)) "-10." -10.)
                (((*read-base* 3.)) "-11." -11.)
                (((*read-base* 3.)) "-13." -13.)
                (nil "+0."  +0.)
                (nil "+1."  +1.)
                (nil "+2."  +2.)
                (nil "+9."  +9.)
                (nil "+10." +10.)
                (nil "+11." +11.)
                (nil "+12." +12.)
                (nil "+19." +19.)
                (((*read-base* 3.)) "+0."  +0.)
                (((*read-base* 3.)) "+1."  +1.)
                (((*read-base* 3.)) "+2."  +2.)
                (((*read-base* 3.)) "+9."  +9.)
                (((*read-base* 3.)) "+10." +10.)
                (((*read-base* 3.)) "+11." +11.)
                (((*read-base* 3.)) "+13." +13.)
                ;; ratio    ::= [sign] {digit}+ slash {digit}+
                (nil "0/0"    nil division-by-zero)
                (nil "1/0"    nil division-by-zero)
                (nil "10/000" nil division-by-zero)
                (nil "0/1" 0)
                (nil "1/1" 1)
                (nil "2/1" 2)
                (nil "20/10" 2)
                (nil "200/100" 2)
                (nil "0/2" 0)
                (nil "1/2" 1/2)
                (nil "0/20" 0)
                (nil "10/20" 1/2)
                (nil "100/200" 1/2)
                (nil "001/2" 1/2)
                (nil "000/20" 0)
                (nil "010/20" 1/2)
                (nil "100/200" 1/2)
                (nil "12345/54321" 12345/54321)
                (nil "+0/0"    nil division-by-zero)
                (nil "+1/0"    nil division-by-zero)
                (nil "+10/000" nil division-by-zero)
                (nil "+0/1" 0)
                (nil "+1/1" 1)
                (nil "+2/1" 2)
                (nil "+20/10" 2)
                (nil "+200/100" 2)
                (nil "+0/2" 0)
                (nil "+1/2" 1/2)
                (nil "+0/20" 0)
                (nil "+10/20" 1/2)
                (nil "+100/200" 1/2)
                (nil "+001/2" 1/2)
                (nil "+000/20" 0)
                (nil "+010/20" 1/2)
                (nil "+100/200" 1/2)
                (nil "+12345/54321" 12345/54321)
                (nil "-0/0"    nil division-by-zero)
                (nil "-1/0"    nil division-by-zero)
                (nil "-10/000" nil division-by-zero)
                (nil "-0/1" -0)
                (nil "-1/1" -1)
                (nil "-2/1" -2)
                (nil "-20/10" -2)
                (nil "-200/100" -2)
                (nil "-0/2" -0)
                (nil "-1/2" -1/2)
                (nil "-0/20" -0)
                (nil "-10/20" -1/2)
                (nil "-100/200" -1/2)
                (nil "-001/2" -1/2)
                (nil "-000/20" -0)
                (nil "-010/20" -1/2)
                (nil "-100/200" -1/2)
                (nil "-12345/54321" -12345/54321)
;;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ exponent
;;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ 
;;; float    ::= [sign] {decimal-digit}+ exponent
;;; float    ::= [sign] {decimal-digit}+ decimal-point {decimal-digit}* exponent
;;; exponent ::=  exponent-marker [sign] {digit}+
;;; 
;;; consing-dot   ::= dot
;;; 
;;; symbol        ::= symbol-name
;;;                 | package-marker symbol-name
;;;                 | package-marker package-marker symbol-name
;;;                 | package-name package-marker symbol-name
;;;                 | package-name package-marker package-marker symbol-name
                )
             :success)
      (multiple-value-bind (val err)
          (ignore-errors
            (eval `(progv
                       ',(mapcar (function first)  (first test))
                       ',(mapcar (function second) (first test))
                     (read-from-string ,(second test)))))
        (assert
         (if (fourth test)
             (typep err (fourth test))
             (eql   val (third test)))
         nil "~S gives ~:[~S~;~:*~S~*~]; expected: ~S"
         `(let ,(first test) (read-from-string ,(second test)))
         err val
         (or (fourth test) (third test)))))))


(defun test-cases (test-name cases)
  (dolist (test cases :success)
    (destructuring-bind (expression expected-values expected-error) test
      (multiple-value-bind (actual-values actual-error)
          (ignore-errors (multiple-value-list (eval expression)))
        (assert (or (and (null expected-error) (null actual-error))
                    (typep actual-error expected-error))
                () "Test ~A~%Testing ~S, expected ~
                      ~:[no error~;an error of type ~:*~S~], ~
                      got this error: ~A"
                test-name expression expected-error actual-error)
        (assert (equalp expected-values actual-values)
                () "Test ~A~%Testing ~S, expected ~S, got ~S" expression
                test-name expected-values actual-values)))))


(defmacro tests (&rest cases)
  (if (stringp (first cases))
      `(test-cases ,(first cases) ',(rest cases))
      `(test-cases "unamed" ',cases)))


(test-extract)
(test-reader)


(tests "symbols"
       ((read-from-string "( abc ab a || |a| |ab| |a b c| )")
        ((ABC AB A || |a| |ab| |a b c|) ;
         32)
        nil))

(let ((*features* '(:a :b :c)))
  (tests "*features*"
   ((eval-feature ':a *standard-input*)           (t)   nil)
   ((eval-feature ':z *standard-input*)           (nil) nil)
   ((eval-feature '42 *standard-input*)           (nil) nil)
   ((eval-feature '(:not :a)    *standard-input*) (nil) nil)
   ((eval-feature '(:not :z)    *standard-input*) (t)   nil)
   ((eval-feature '(:not :a :b) *standard-input*) ()    reader-error)
   ((eval-feature '(:and)       *standard-input*) (t)   nil)
   ((eval-feature '(:and :a)    *standard-input*) (t)   nil)
   ((eval-feature '(:and :a :b) *standard-input*) (t)   nil)
   ((eval-feature '(:and :a :c) *standard-input*) (t)   nil)
   ((eval-feature '(:and :a :z) *standard-input*) (nil) nil)
   ((eval-feature '(:and :y :z) *standard-input*) (nil) nil)
   ((eval-feature '(:or)        *standard-input*) (nil) nil)
   ((eval-feature '(:or :a)     *standard-input*) (t)   nil)
   ((eval-feature '(:or :a :b)  *standard-input*) (t)   nil)
   ((eval-feature '(:or :a :c)  *standard-input*) (t)   nil)
   ((eval-feature '(:or :a :z)  *standard-input*) (t)   nil)
   ((eval-feature '(:or :y :z)  *standard-input*) (nil) nil)
   ((eval-feature '(:or (:and :a (:not :z)) (:and (:not :a) :z))
                  *standard-input*)               (t)   nil)
   ((eval-feature '(:and (:or :a (:not :z)) (:or (:not :a) :z))
                  *standard-input*)               (nil) nil)
   ((eval-feature '(:and :a :b (:or :y :z (:not :a)))
                  *standard-input*)               (nil) nil)
   ((eval-feature '(:and :a :b (:or :y :z (:not 42)))
                  *standard-input*)               (t)   nil)))



(tests "lists"
 ((read-from-string "()")                       (() 2)           nil)
 ((read-from-string "(a)")                      ((a) 3)          nil)
 ((read-from-string "(a b)")                    ((a b) 5)        nil)
 ((read-from-string "(a b c)")                  ((a b c) 7)      nil)
 ((read-from-string "(a b c d)")                ((a b c d) 9)    nil)
 ((read-from-string "(a b c . d)")              ((a b c . d) 11)  nil)
 ((read-from-string "(a b c . d e)")            nil            reader-error)
 ((read-from-string "(a b c . . d)")            nil            reader-error)
 ((read-from-string "(a b c . d .)")            nil            reader-error)
 ((let ((*features* '(:test)))
    (read-from-string "(a b c #+test d)"))      ((a b c d) 16)    nil)
 ((let ((*features* '(:test)))
    (read-from-string "(a b c #-test d)"))      ((a b c) 16)      nil)
 ((let ((*features* '(:test)))
    (read-from-string "(a b c . #+test d)"))    ((a b c . d) 18)  nil)
 ((let ((*features* '(:test)))
    (read-from-string "(a b c . #-test d e)"))  ((a b c . e) 20)  nil)
 ((let ((*features* '(:test)))
    (read-from-string "(a b c #+test . d)"))    ((a b c . d) 18)  nil)
 ((let ((*features* '(:test)))
    (read-from-string "(a b c #-test . d)"))    ((a b c d) 18)    nil)
 ((read-from-string "(#+(or) #$foo       xyz)") nil               reader-error)
 ((read-from-string "(#+(or) abc:def:ghi xyz)") ((xyz) 24)        nil))


(tests "#+ with #= and ##"
 ((let ((*features* (quote (:a :b))))
    (read-from-string "(#+#1=(or a b) #1#)"))
  (((:or :a :b)) 19)
  nil)
 ((let ((*features* (quote (:a :b))))
    (read-from-string "(#+#.(cl:if (cl:eq :a (cl:first cl:*features*)) '(:and) '(:or)) equal)"))
  ((equal) 70)
  nil))


#- (and)
(tests 
 ((let ((*features* (quote (:a :b)))) 
    (read-from-string "#+#1=(or a b) #1#"))
  ((:OR :A :B) 44)
  nil))


(tests "bit vectors, numbers, and pathnames"
 ((read-from-string "(#*101111 #6*10111110101 #6*101111 #6*1010 #6*1011 #* #0*11010)")
  ((#*101111 #*101111 #*101111 #*101000 #*101111 #* #*) 63)
  nil)
 ((read-from-string "(#b10111101 #o275 #xbd #36r59)")
  ((189 189 189 189) 30)
  nil)
 ((read-from-string "#P\"/tmp/a.c\"")
  (#.(make-pathname :directory '(:absolute "tmp")
                     :name "a" :type "c" :version nil
                     :case :local) 12)
  nil))

#- (and)
(tests
 ((progn
    (defstruct s a b c) (read-from-string "#S(s a 1 b 2 c 3)"))
  (#S(S :A 1 :B 2 :C 3) 17)
  nil))


(tests "complex numbers"
 ((read-from-string "( #C(123 456) #c(-123 456)
                       #C(12.3 456) #c(-123 45.6)
                       #C(123/10 456/100) #c(-123/10 456/100) )")
  (( #C(123 456) #c(-123 456)
                       #C(12.3 456) #c(-123 45.6)
                       #C(123/10 456/100) #c(-123/10 456/100) )
   140)
  nil))



(tests "read-delimited-list with comments"
 ((with-input-from-string (src " \"!A\"
) def)
")
    (values (READ-delimited-list #\) src)
            (READ-delimited-list #\) src)))
  (("!A") (DEF))
  nil)

 ((with-input-from-string (src "#( \"!A\" 
) (def)
")
    (values (READ src)
            (READ src)))
  (#("!A") (DEF))
  nil)

 ((with-input-from-string (src "( \"!A\"
) (def)
")
    (values (READ src)
            (READ src)))
  (("!A") (DEF))
  nil)

 ((with-input-from-string (src " \"!A\" ; comment
) def)
")
    (values (READ-delimited-list #\) src)
            (READ-delimited-list #\) src)))
  (("!A") (DEF))
  nil)
 
  ((with-input-from-string (src "#( \"!A\"  ; comment
) (def)
")
    (values (READ src)
            (READ src)))
  (#("!A") (DEF))
  nil)

  ((with-input-from-string (src "( \"!A\" ; comment
) (def)
")
    (values (READ src)
            (READ src)))
  (("!A") (DEF))
  nil))


(tests "lists with comments"
 ((read-from-string "( () (a) (a b) (a b c) (a . ()) (a . b) (a b . ()) (a b . c)
                       ( ; comment
                    ) (a ; comment
                    ) (a ; comment
               b) (a b c ; comment
                    ) (a ; comment
              . ()) (a . ; comment
                  ()) (a ; comment
               . b) (a . ; comment
               b) (a . b ; comment
                 ) (a b .; comment
             ()) (a b . c;comment
                      ))")
  ((NIL (A) (A B) (A B C) (A) (A . B) (A B) (A B . C) NIL (A) (A B) (A B C) (A)
   (A) (A . B) (A . B) (A . B) (A B) (A B . C)) 
   469)
  nil))


(tests "vector with too much data"
       ((with-input-from-string (input "#2(a b c) d e")
          (values (read input) (read-line input)))
        (#(A B)                         
          " d e")
        nil))

(tests "non-empty vector with too liltle data"
       ((length (read-from-string "#2()"))
        (2)
        nil))

(tests "vectors and vectors with comments"
       ((read-from-string "( #() #(a) #(a b) #(a b c) #(a  #()) #(a  b) #(a b  #()) #(a b  c)
                              #2(a) #2(a b) #2(a b c) #2(a  #()) #2(a  b) #2(a b  #()) #2(a b  c)
                       #( ; comment
                    ) #(a ; comment
                    ) #(a ; comment
               b) #(a b c ; comment
                    ) #(a ; comment
               #()) #(a  ; comment
                  #()) #(a ; comment
                b) #(a  ; comment
               b) #(a  b ; comment
                 ) #(a b ; comment
             #()) #(a b  c;comment
                      ))")
        ((#() #(A) #(A B) #(A B C) #(A #()) #(A B) #(A B #()) #(A B C)
           #(A A) #(A B) #(A B) #(A #()) #(A B) #(A B) #(A B) #() #(A)
           #(A B) #(A B C) #(A #()) #(A #()) #(A B) #(A B) #(A B) #(A B #())
           #(A B C))
         580)
        nil))



(defun check-symbols ()
  (dolist (sym '("READTABLE"
                 "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
                 "READ" "READ-PRESERVING-WHITESPACE"
                 "READ-DELIMITED-LIST"
                 "READ-FROM-STRING"
                 "READTABLE-CASE" "READTABLEP"
                 "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
                 "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
                 "SET-SYNTAX-FROM-CHAR"
                 "WITH-STANDARD-IO-SYNTAX"
                 "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
                 "*READ-SUPPRESS*" "*READTABLE*")
           :success)
    (let ((s-here (find-symbol sym *package*))
          (s-cl   (find-symbol sym "COMMON-LISP")))
      (assert (not (eq s-here s-cl))
              () "The symbol ~S is interned both in COMMON-LISP and in ~A"
              s-here (package-name *package*)))))

(check-symbols)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

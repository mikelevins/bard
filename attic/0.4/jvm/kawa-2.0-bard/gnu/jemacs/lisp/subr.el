;;; subr.el --- basic lisp subroutines for XEmacs

;; Copyright (C) 1985, 1986, 1992, 1994-5, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: extensions, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with XEmacs.

;; There's not a whole lot in common now with the FSF version,
;; be wary when applying differences.  I've left in a number of lines
;; of commentary just to give diff(1) something to synch itself with to
;; provide useful context diffs. -sb

;;; Code:


;;;; Lisp language features.

#|
(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
  `(function (lambda ,@cdr)))

(defmacro defun-when-void (&rest args)
  "Define a function, just like `defun', unless it's already defined.
Used for compatibility among different emacs variants."
  `(if (fboundp ',(car args))
       nil
     (defun ,@args)))

(defmacro define-function-when-void (&rest args)
  "Define a function, just like `define-function', unless it's already defined.
Used for compatibility among different emacs variants."
  `(if (fboundp ,(car args))
       nil
     (define-function ,@args)))
|#


;;;; Keymap support.
;; XEmacs: removed to keymap.el

;;;; The global keymap tree.

;;; global-map, esc-map, and ctl-x-map have their values set up in
;;; keymap.c; we just give them docstrings here.

;;;; Event manipulation functions.

;; XEmacs: This stuff is done in C Code.

;;;; Obsolescent names for functions.
;; XEmacs: not used.

;; XEmacs:
(defun local-variable-if-set-p (sym buffer)
  "Return t if SYM would be local to BUFFER after it is set.
A nil value for BUFFER is *not* the same as (current-buffer), but
can be used to determine whether `make-variable-buffer-local' has been
called on SYM."
  (local-variable-p sym buffer t))


;;;; Hook manipulation functions.

;; (defconst run-hooks 'run-hooks ...)

(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function works by making `t' a member of the buffer-local value,
which acts as a flag to run the hook functions in the default value as
well.  This works for all normal hooks, but does not work for most
non-normal hooks yet.  We will be changing the callers of non-normal
hooks so that they can handle localness; this has to be done one by
one.

This function does nothing if HOOK is already local in the current
buffer.

Do not use `make-local-variable' to make a hook variable buffer-local.

See also `add-local-hook' and `remove-local-hook'."
  (if (local-variable-p hook (current-buffer)) ; XEmacs
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t))))

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.

You can remove this hook yourself using `remove-hook'.

See also `add-local-hook' and `add-one-shot-hook'."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (if (or local
	  ;; Detect the case where make-local-variable was used on a hook
	  ;; and do what we used to do.
	  (and (local-variable-if-set-p hook (current-buffer)) ; XEmacs
	       (not (memq t (symbol-value hook)))))
      ;; Alter the local value only.
      (or (if (consp function)
	      (member function (symbol-value hook))
	    (memq function (symbol-value hook)))
	  (set hook
	       (if append
		   (append (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))
    ;; Alter the global value (which is also the only value,
    ;; if the hook doesn't have a local value).
    (or (if (consp function)
	    (member function (default-value hook))
	  (memq function (default-value hook)))
	(set-default hook
		     (if append
			 (append (default-value hook) (list function))
		       (cons function (default-value hook)))))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (not (default-boundp 'hook))
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (flet ((hook-remove
	    (function hook-value)
	    (flet ((hook-test
		    (fn hel)
		    (or (equal fn hel)
			(and (symbolp hel)
			     (equal fn
				    (get hel 'one-shot-hook-fun))))))
	      (if (and (consp hook-value)
		       (not (functionp hook-value)))
		  (if (member* function hook-value :test 'hook-test)
		      (setq hook-value
			    (delete* function (copy-sequence hook-value)
				     :test 'hook-test)))
		(if (equal hook-value function)
		    (setq hook-value nil)))
	      hook-value)))
      (if (or local
	      ;; Detect the case where make-local-variable was used on a hook
	      ;; and do what we used to do.
	      (and (local-variable-p hook (current-buffer))
		   (not (memq t (symbol-value hook)))))
	  (set hook (hook-remove function (symbol-value hook)))
	(set-default hook (hook-remove function (default-value hook)))))))

;; XEmacs addition
;; #### we need a coherent scheme for indicating compatibility info,
;; so that it can be programmatically retrieved.
(defun add-local-hook (hook function &optional append)
  "Add to the local value of HOOK the function FUNCTION.
This modifies only the buffer-local value for the hook (which is
automatically make buffer-local, if necessary), not its default value.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.

You can remove this hook yourself using `remove-local-hook'.

See also `add-hook' and `make-local-hook'."
  (make-local-hook hook)
  (add-hook hook function append t))

;; XEmacs addition
(defun remove-local-hook (hook function)
  "Remove from the local value of HOOK the function FUNCTION.
This modifies only the buffer-local value for the hook, not its default
value. (Nothing happens if the hook is not buffer-local.)
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

See also `add-local-hook' and `make-local-hook'."
  (if (local-variable-p hook (current-buffer))
      (remove-hook hook function t)))

(defun add-one-shot-hook (hook function &optional append local)
  "Add to the value of HOOK the one-shot function FUNCTION.
FUNCTION will automatically be removed from the hook the first time
after it runs (whether to completion or to an error).
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.

You can remove this hook yourself using `remove-hook'.

See also `add-hook', `add-local-hook', and `add-local-one-shot-hook'."
  (let ((sym (gensym)))
    (fset sym `(lambda (&rest args)
		 (unwind-protect
		     (apply ',function args)
		   (remove-hook ',hook ',sym ',local))))
    (put sym 'one-shot-hook-fun function)
    (add-hook hook sym append local)))

(defun add-local-one-shot-hook (hook function &optional append)
  "Add to the local value of HOOK the one-shot function FUNCTION.
FUNCTION will automatically be removed from the hook the first time
after it runs (whether to completion or to an error).
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.

You can remove this hook yourself using `remove-local-hook'.

See also `add-hook', `add-local-hook', and `add-local-one-shot-hook'."
  (make-local-hook hook)
  (add-one-shot-hook hook function append t))

(defun add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (or (member element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var)))))

;; XEmacs additions
;; called by Fkill_buffer()
(defvar kill-buffer-hook nil
  "Function or functions to be called when a buffer is killed.
The value of this variable may be buffer-local.
The buffer about to be killed is current when this hook is run.")

;; called by Frecord_buffer()
(defvar record-buffer-hook nil
  "Function or functions to be called when a buffer is recorded.
The value of this variable may be buffer-local.
The buffer being recorded is passed as an argument to the hook.")

;; in C in FSFmacs
(defvar kill-emacs-hook nil
  "Function or functions to be called when `kill-emacs' is called,
just before emacs is actually killed.")

;; not obsolete.
;; #### These are a bad idea, because the CL RPLACA and RPLACD
;; return the cons cell, not the new CAR/CDR.         -hniksic
;; The proper definition would be:
;; (defun rplaca (conscell newcar)
;;   (setcar conscell newcar)
;;   conscell)
;; ...and analogously for RPLACD.
(define-function 'rplaca 'setcar)
(define-function 'rplacd 'setcdr)

(defun copy-symbol (symbol &optional copy-properties)
  "Return a new uninterned symbol with the same name as SYMBOL.
If COPY-PROPERTIES is non-nil, the new symbol will have a copy of
SYMBOL's value, function, and property lists."
  (let ((new (make-symbol (symbol-name symbol))))
    (when copy-properties
      ;; This will not copy SYMBOL's chain of forwarding objects, but
      ;; I think that's OK.  Callers should not expect such magic to
      ;; keep working in the copy in the first place.
      (and (boundp symbol)
	   (set new (symbol-value symbol)))
      (and (fboundp symbol)
	   (fset new (symbol-function symbol)))
      (setplist new (copy-list (symbol-plist symbol))))
    new))

;;;; String functions.

;; XEmacs
(defun replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (check-argument-type 'stringp str)
  (check-argument-type 'stringp newtext)
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond (literal newtext)
		    (t (mapconcat
			(lambda (c)
			  (if special
			      (progn
				(setq special nil)
				(cond ((eq c ?\\) "\\")
				      ((eq c ?&)
				       (substring str
						  (match-beginning 0)
						  (match-end 0)))
				      ((and (>= c ?0) (<= c ?9))
				       (if (> c (+ ?0 (length
						       (match-data))))
					   ;; Invalid match num
					   (error "Invalid match num: %c" c)
					 (setq c (- c ?0))
					 (substring str
						    (match-beginning c)
						    (match-end c))))
				      (t (char-to-string c))))
			    (if (eq c ?\\) (progn (setq special t) nil)
			      (char-to-string c))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))

(defun split-string (string &optional pattern)
  "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  (let (parts (start 0) (len (length string)))
    (if (string-match pattern string)
	(setq parts (cons (substring string 0 (match-beginning 0)) parts)
	      start (match-end 0)))
    (while (and (< start len)
		(string-match pattern string (if (> start (match-beginning 0))
						 start
					       (1+ start))))
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

;; #### #### #### AAaargh!  Must be in C, because it is used insanely
;; early in the bootstrap process.
;(defun split-path (path)
;  "Explode a search path into a list of strings.
;The path components are separated with the characters specified
;with `path-separator'."
;  (while (or (not stringp path-separator)
;	     (/= (length path-separator) 1))
;    (setq path-separator (signal 'error (list "\
;`path-separator' should be set to a single-character string"
;					      path-separator))))
;  (split-string-by-char path (aref separator 0)))

#|
(defmacro with-output-to-string (&rest forms)
  "Collect output to `standard-output' while evaluating FORMS and return
it as a string."
  ;; by "William G. Dubuque" <wgd@zurich.ai.mit.edu> w/ mods from Stig
  `(with-current-buffer (get-buffer-create
			 (generate-new-buffer-name " *string-output*"))
     (setq buffer-read-only nil)
     (buffer-disable-undo (current-buffer))
     (erase-buffer)
     (let ((standard-output (current-buffer)))
       ,@forms)
     (prog1
	 (buffer-string)
       (erase-buffer))))

(defmacro with-current-buffer (buffer &rest body)
  "Temporarily make BUFFER the current buffer and execute the forms in BODY.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  `(save-current-buffer
    (set-buffer ,buffer)
    ,@body))

(defmacro with-temp-file (file &rest forms)
  "Create a new buffer, evaluate FORMS there, and write the buffer to FILE.
The value of the last form in FORMS is returned, like `progn'.
See also `with-temp-buffer'."
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-file ,file)
	   (,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp file*"))))
       (unwind-protect
	   (prog1
	       (with-current-buffer ,temp-buffer
		 ,@forms)
	     (with-current-buffer ,temp-buffer
               (widen)
	       (write-region (point-min) (point-max) ,temp-file nil 0)))
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp*"))))
       (unwind-protect
	   (with-current-buffer ,temp-buffer
	     ,@forms)
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

;; Moved from mule-coding.el.
(defmacro with-string-as-buffer-contents (str &rest body)
  "With the contents of the current buffer being STR, run BODY.
Returns the new contents of the buffer, as modified by BODY.
The original current buffer is restored afterwards."
  `(with-temp-buffer
     (insert ,str)
     ,@body
     (buffer-string)))
|#

(defun insert-face (string face)
  "Insert STRING and highlight with FACE.  Return the extent created."
  (let ((p (point)) ext)
    (insert string)
    (setq ext (make-extent p (point)))
    (set-extent-face ext face)
    ext))

;; not obsolete.
(define-function 'string= 'string-equal)
(define-function 'string< 'string-lessp)
(define-function 'int-to-string 'number-to-string)
(define-function 'string-to-int 'string-to-number)

;; These two names are a bit awkward, as they conflict with the normal
;; foo-to-bar naming scheme, but CLtL2 has them, so they stay.
(define-function 'char-int 'char-to-int)
(define-function 'int-char 'int-to-char)


;; alist/plist functions
(defun plist-to-alist (plist)
  "Convert property list PLIST into the equivalent association-list form.
The alist is returned.  This converts from

\(a 1 b 2 c 3)

into

\((a . 1) (b . 2) (c . 3))

The original plist is not modified.  See also `destructive-plist-to-alist'."
  (let (alist)
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun destructive-plist-to-alist (plist)
  "Convert property list PLIST into the equivalent association-list form.
The alist is returned.  This converts from

\(a 1 b 2 c 3)

into

\((a . 1) (b . 2) (c . 3))

The original plist is destroyed in the process of constructing the alist.
See also `plist-to-alist'."
  (let ((head plist)
	next)
    (while plist
      ;; remember the next plist pair.
      (setq next (cddr plist))
      ;; make the cons holding the property value into the alist element.
      (setcdr (cdr plist) (cadr plist))
      (setcar (cdr plist) (car plist))
      ;; reattach into alist form.
      (setcar plist (cdr plist))
      (setcdr plist next)
      (setq plist next))
    head))

(defun alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

;; getf, remf in cl*.el.

#|
(defmacro putf (plist prop val)
  "Add property PROP to plist PLIST with value VAL.
Analogous to (setq PLIST (plist-put PLIST PROP VAL))."
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defmacro laxputf (lax-plist prop val)
  "Add property PROP to lax plist LAX-PLIST with value VAL.
Analogous to (setq LAX-PLIST (lax-plist-put LAX-PLIST PROP VAL))."
  `(setq ,lax-plist (lax-plist-put ,lax-plist ,prop ,val)))

(defmacro laxremf (lax-plist prop)
  "Remove property PROP from lax plist LAX-PLIST.
Analogous to (setq LAX-PLIST (lax-plist-remprop LAX-PLIST PROP))."
  `(setq ,lax-plist (lax-plist-remprop ,lax-plist ,prop)))
|#

;;; Error functions

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'.
This error is not continuable: you cannot continue execution after the
error using the debugger `r' command.  See also `cerror'."
  (while t
    (apply 'cerror args)))

(defun cerror (&rest args)
  "Like `error' but signals a continuable error."
  (signal 'error (list (apply 'format args))))

#|
(defmacro check-argument-type (predicate argument)
  "Check that ARGUMENT satisfies PREDICATE.
If not, signal a continuable `wrong-type-argument' error until the
returned value satisfies PREDICATE, and assign the returned value
to ARGUMENT."
  `(if (not (,(eval predicate) ,argument))
       (setq ,argument
	     (wrong-type-argument ,predicate ,argument))))
|#

(defun signal-error (error-symbol data)
  "Signal a non-continuable error.  Args are ERROR-SYMBOL, and associated DATA.
An error symbol is a symbol defined using `define-error'.
DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See also `signal', and the functions to handle errors: `condition-case'
and `call-with-condition-handler'."
  (while t
    (signal error-symbol data)))

(defun define-error (error-sym doc-string &optional inherits-from)
  "Define a new error, denoted by ERROR-SYM.
DOC-STRING is an informative message explaining the error, and will be
printed out when an unhandled error occurs.
ERROR-SYM is a sub-error of INHERITS-FROM (which defaults to `error').

\[`define-error' internally works by putting on ERROR-SYM an `error-message'
property whose value is DOC-STRING, and an `error-conditions' property
that is a list of ERROR-SYM followed by each of its super-errors, up
to and including `error'.  You will sometimes see code that sets this up
directly rather than calling `define-error', but you should *not* do this
yourself.]"
  (check-argument-type 'symbolp error-sym)
  (check-argument-type 'stringp doc-string)
  (put error-sym 'error-message doc-string)
  (or inherits-from (setq inherits-from 'error))
  (let ((conds (get inherits-from 'error-conditions)))
    (or conds (signal-error 'error (list "Not an error symbol" error-sym)))
    (put error-sym 'error-conditions (cons error-sym conds))))

;;;; Miscellanea.

;; This is now in C.
;(defun buffer-substring-no-properties (beg end)
;  "Return the text from BEG to END, without text properties, as a string."
;  (let ((string (buffer-substring beg end)))
;    (set-text-properties 0 (length string) nil string)
;    string))

(defun get-buffer-window-list (&optional buffer minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
BUFFER defaults to the current buffer.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (cond ((null buffer)
	 (setq buffer (current-buffer)))
	((not (bufferp buffer))
	 (setq buffer (get-buffer buffer))))
  (let (windows)
    (walk-windows (lambda (window)
		    (if (eq (window-buffer window) buffer)
			(push window windows)))
		  minibuf frame)
    windows))

(defun ignore (&rest ignore)
  "Do nothing and return nil.
This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

(define-function 'eval-in-buffer 'with-current-buffer)
(make-obsolete 'eval-in-buffer 'with-current-buffer)

;;; The real defn is in abbrev.el but some early callers
;;;  (eg lisp-mode-abbrev-table) want this before abbrev.el is loaded...

(if (not (fboundp 'define-abbrev-table))
    (progn
      (setq abbrev-table-name-list '())
      (fset 'define-abbrev-table (function (lambda (name defs)
                                   ;; These are fixed-up when abbrev.el loads.
                                   (setq abbrev-table-name-list
                                         (cons (cons name defs)
                                               abbrev-table-name-list)))))))

;;; `functionp' has been moved into C.

;;(defun functionp (object)
;;  "Non-nil if OBJECT can be called as a function."
;;  (or (and (symbolp object) (fboundp object))
;;      (subrp object)
;;      (compiled-function-p object)
;;      (eq (car-safe object) 'lambda)))



(defun function-interactive (function)
  "Return the interactive specification of FUNCTION.
FUNCTION can be any funcallable object.
The specification will be returned as the list of the symbol `interactive'
 and the specs.
If FUNCTION is not interactive, nil will be returned."
  (setq function (indirect-function function))
  (cond ((compiled-function-p function)
	 (compiled-function-interactive function))
	((subrp function)
	 (subr-interactive function))
	((eq (car-safe function) 'lambda)
	 (let ((spec (if (stringp (nth 2 function))
			 (nth 3 function)
		       (nth 2 function))))
	   (and (eq (car-safe spec) 'interactive)
		spec)))
	(t
	 (error "Non-funcallable object: %s" function))))

;; This function used to be an alias to `buffer-substring', except
;; that FSF Emacs 20.4 added a BUFFER argument in an incompatible way.
;; The new FSF's semantics makes more sense, but we try to support
;; both for backward compatibility.
(defun buffer-string (&optional buffer old-end old-buffer)
  "Return the contents of the current buffer as a string.
If narrowing is in effect, this function returns only the visible part
of the buffer.

If BUFFER is specified, the contents of that buffer are returned.

The arguments OLD-END and OLD-BUFFER are supported for backward
compatibility with pre-21.2 XEmacsen times when arguments to this
function were (buffer-string &optional START END BUFFER)."
  (cond
   ((or (stringp buffer) (bufferp buffer))
    ;; Most definitely the new way.
    (buffer-substring nil nil buffer))
   ((or (stringp old-buffer) (bufferp old-buffer)
	(natnump buffer) (natnump old-end))
    ;; Definitely the old way.
    (buffer-substring buffer old-end old-buffer))
   (t
    ;; Probably the old way.
    (buffer-substring buffer old-end old-buffer))))

;; This was not present before.  I think Jamie had some objections
;; to this, so I'm leaving this undefined for now. --ben

;;; The objection is this: there is more than one way to load the same file.
;;; "foo", "foo.elc", "foo.el", and "/some/path/foo.elc" are all different
;;; ways to load the exact same code.  `eval-after-load' is too stupid to
;;; deal with this sort of thing.  If this sort of feature is desired, then
;;; it should work off of a hook on `provide'.  Features are unique and
;;; the arguments to (load) are not.  --Stig

;; We provide this for FSFmacs compatibility, at least until we devise
;; something better.

;;;; Specifying things to do after certain files are loaded.

(defun eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
  ;; Make sure there is an element for FILE.
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  ;; Add FORM to the element if it isn't there.
  (let ((elt (assoc file after-load-alist)))
    (or (member form (cdr elt))
	(progn
	  (nconc elt (list form))
	  ;; If the file has been loaded already, run FORM right away.
	  (and (assoc file load-history)
	       (eval form)))))
  form)
(make-compatible 'eval-after-load "")

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))
(make-compatible 'eval-next-after-load "")

; alternate names (not obsolete)
(if (not (fboundp 'mod)) (define-function 'mod '%))
(define-function 'move-marker 'set-marker)
(define-function 'beep 'ding)  ; preserve lingual purity
(define-function 'indent-to-column 'indent-to)
(define-function 'backward-delete-char 'delete-backward-char)
(define-function 'search-forward-regexp (symbol-function 're-search-forward))
(define-function 'search-backward-regexp (symbol-function 're-search-backward))
(define-function 'remove-directory 'delete-directory)
(define-function 'set-match-data 'store-match-data)
(define-function 'send-string-to-terminal 'external-debugging-output)

;;; subr.el ends here

;;; simple.el --- basic editing commands for XEmacs

;; Copyright (C) 1985-7, 1993-5, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: lisp, extensions, internal, dumped

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

;;; Synched up with: FSF 19.34 [But not very closely].

;;; Commentary:

;; This file is dumped with XEmacs.

;; A grab-bag of basic XEmacs commands not specifically related to some
;; major mode or to file-handling.

;; Changes for zmacs-style active-regions:
;;
;; beginning-of-buffer, end-of-buffer, count-lines-region,
;; count-lines-buffer, what-line, what-cursor-position, set-goal-column,
;; set-fill-column, prefix-arg-internal, and line-move (which is used by
;; next-line and previous-line) set zmacs-region-stays to t, so that they
;; don't affect the current region-hilighting state.
;;
;; mark-whole-buffer, mark-word, exchange-point-and-mark, and
;; set-mark-command (without an argument) call zmacs-activate-region.
;;
;; mark takes an optional arg like the new Fmark_marker() does.  When
;; the region is not active, mark returns nil unless the optional arg is true.
;;
;; push-mark, pop-mark, exchange-point-and-mark, and set-marker, and
;; set-mark-command use (mark t) so that they can access the mark whether
;; the region is active or not.
;;
;; shell-command, shell-command-on-region, yank, and yank-pop (which all
;; push a mark) have been altered to call exchange-point-and-mark with an
;; argument, meaning "don't activate the region".  These commands  only use
;; exchange-point-and-mark to position the newly-pushed mark correctly, so
;; this isn't a user-visible change.  These functions have also been altered
;; to use (mark t) for the same reason.

;; 97/3/14 Jareth Hein (jhod@po.iijnet.or.jp) added kinsoku processing (support
;; for filling of Asian text) into the fill code. This was ripped bleeding from
;; Mule-2.3, and could probably use some feature additions (like additional wrap
;; styles, etc)

;; 97/06/11 Steve Baur (steve@xemacs.org) Convert use of
;;  (preceding|following)-char to char-(after|before).

;;; Code:

(defgroup editing-basics nil
  "Most basic editing variables."
  :group 'editing)

(defgroup killing nil
  "Killing and yanking commands."
  :group 'editing)

(defgroup fill-comments nil
  "Indenting and filling of comments."
  :prefix "comment-"
  :group 'fill)

(defgroup paren-matching nil
  "Highlight (un)matching of parens and expressions."
  :prefix "paren-"
  :group 'matching)

(defgroup log-message nil
  "Messages logging and display customizations."
  :group 'minibuffer)

(defgroup warnings nil
  "Warnings customizations."
  :group 'minibuffer)


(defcustom search-caps-disable-folding t
  "*If non-nil, upper case chars disable case fold searching.
This does not apply to \"yanked\" strings."
  :type 'boolean
  :group 'editing-basics)

;; This is stolen (and slightly modified) from FSF emacs's
;; `isearch-no-upper-case-p'.
(defun no-upper-case-p (string &optional regexp-flag)
  "Return t if there are no upper case chars in STRING.
If REGEXP-FLAG is non-nil, disregard letters preceded by `\\' (but not `\\\\')
since they have special meaning in a regexp."
  (let ((case-fold-search nil))
    (not (string-match (if regexp-flag 
			   "\\(^\\|\\\\\\\\\\|[^\\]\\)[A-Z]"
			 "[A-Z]")
		       string))
    ))

#|
(defmacro with-search-caps-disable-folding (string regexp-flag &rest body) "\
Eval BODY with `case-fold-search' let to nil if `search-caps-disable-folding' 
is non-nil, and if STRING (either a string or a regular expression according
to REGEXP-FLAG) contains uppercase letters."
  `(let ((case-fold-search
          (if (and case-fold-search search-caps-disable-folding)
              (no-upper-case-p ,string ,regexp-flag)
            case-fold-search)))
     ,@body))
(put 'with-search-caps-disable-folding 'lisp-indent-function 2)
(put 'with-search-caps-disable-folding 'edebug-form-spec 
     '(sexp sexp &rest form))

(defmacro with-interactive-search-caps-disable-folding (string regexp-flag 
							       &rest body)
  "Same as `with-search-caps-disable-folding', but only in the case of a
function called interactively."
  `(let ((case-fold-search
	  (if (and (interactive-p) 
		   case-fold-search search-caps-disable-folding)
              (no-upper-case-p ,string ,regexp-flag)
            case-fold-search)))
     ,@body))
(put 'with-interactive-search-caps-disable-folding 'lisp-indent-function 2)
(put 'with-interactive-search-caps-disable-folding 'edebug-form-spec 
     '(sexp sexp &rest form))
|#

(defun newline (&optional arg)
  (interactive "*P")
  (insert-char ?\C-j arg))
#|
(defun newline (&optional arg)
  "Insert a newline, and move to left margin of the new line if it's blank.
The newline is marked with the text-property `hard'.
With arg, insert that many newlines.
In Auto Fill mode, if no numeric arg, break the preceding line if it's long."
  (interactive "*P")
  (barf-if-buffer-read-only nil (point))
  ;; Inserting a newline at the end of a line produces better redisplay in
  ;; try_window_id than inserting at the beginning of a line, and the textual
  ;; result is the same.  So, if we're at beginning of line, pretend to be at
  ;; the end of the previous line.
  ;; #### Does this have any relevance in XEmacs?
  (let ((flag (and (not (bobp))
		   (bolp)
		   ;; Make sure the newline before point isn't intangible.
		   (not (get-char-property (1- (point)) 'intangible))
		   ;; Make sure the newline before point isn't read-only.
		   (not (get-char-property (1- (point)) 'read-only))
		   ;; Make sure the newline before point isn't invisible.
		   (not (get-char-property (1- (point)) 'invisible))
		   ;; This should probably also test for the previous char
		   ;;  being the *last* character too.
		   (not (get-char-property (1- (point)) 'end-open))
		   ;; Make sure the newline before point has the same
		   ;; properties as the char before it (if any).
		   (< (or (previous-extent-change (point)) -2)
		      (- (point) 2))))
	(was-page-start (and (bolp)
			     (looking-at page-delimiter)))
	(beforepos (point)))
    (if flag (backward-char 1))
    ;; Call self-insert so that auto-fill, abbrev expansion etc. happens.
    ;; Set last-command-char to tell self-insert what to insert.
    (let ((last-command-char ?\n)
	  ;; Don't auto-fill if we have a numeric argument.
	  ;; Also not if flag is true (it would fill wrong line);
	  ;; there is no need to since we're at BOL.
	  (auto-fill-function (if (or arg flag) nil auto-fill-function)))
      (unwind-protect
	  (self-insert-command (prefix-numeric-value arg))
	;; If we get an error in self-insert-command, put point at right place.
	(if flag (forward-char 1))))
    ;; If we did *not* get an error, cancel that forward-char.
    (if flag (backward-char 1))
    ;; Mark the newline(s) `hard'.
    (if use-hard-newlines
	(let* ((from (- (point) (if arg (prefix-numeric-value arg) 1)))
	       (sticky (get-text-property from 'end-open))) ; XEmacs
	  (put-text-property from (point) 'hard 't)
	  ;; If end-open is not "t", add 'hard to end-open list
	  (if (and (listp sticky) (not (memq 'hard sticky)))
	      (put-text-property from (point) 'end-open ; XEmacs
				 (cons 'hard sticky)))))
    ;; If the newline leaves the previous line blank,
    ;; and we have a left margin, delete that from the blank line.
    (or flag
	(save-excursion
	  (goto-char beforepos)
	  (beginning-of-line)
	  (and (looking-at "[ \t]$")
	       (> (current-left-margin) 0)
	       (delete-region (point) (progn (end-of-line) (point))))))
    (if flag (forward-char 1))
    ;; Indent the line after the newline, except in one case:
    ;; when we added the newline at the beginning of a line
    ;; which starts a page.
    (or was-page-start
	(move-to-left-margin nil t)))
  nil)

(defun set-hard-newline-properties (from to)
  (let ((sticky (get-text-property from 'rear-nonsticky)))
    (put-text-property from to 'hard 't)
    ;; If rear-nonsticky is not "t", add 'hard to rear-nonsticky list
    (if (and (listp sticky) (not (memq 'hard sticky)))
	(put-text-property from (point) 'rear-nonsticky
			   (cons 'hard sticky)))))
|#

(defun open-line (arg)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a left-margin, insert them on the new line
if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point)))
    (newline arg)
    (goto-char loc)
    (while (> arg 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert fill-prefix))))
      (forward-line 1)
      (setq arg (1- arg)))
    (goto-char loc)
    (end-of-line)))

(defun split-line ()
  "Split current line, moving portion beyond point vertically down."
  (interactive "*")
  (skip-chars-forward " \t")
  (let ((col (current-column))
	(pos (point)))
    (newline 1)
    (indent-to col 0)
    (goto-char pos)))

(defun quoted-insert (arg)
  "Read next input character and insert it.
This is useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code.

In overwrite mode, this function inserts the character anyway, and
does not handle octal digits specially.  This means that if you use
overwrite as your normal editing mode, you can use this function to
insert characters when necessary.

In binary overwrite mode, this function does overwrite, and octal
digits are interpreted as a character code.  This is supposed to make
this function useful in editing binary files."
  (interactive "*p")
  (let ((char (if (or (not overwrite-mode)
		      (eq overwrite-mode 'overwrite-mode-binary))
		  (read-quoted-char)
		;; read-char obeys C-g, so we should protect.  FSF
		;; doesn't have the protection here, but it's a bug in
		;; FSF.
		(let ((inhibit-quit t))
		  (read-char)))))
    (if (> arg 0)
	(if (eq overwrite-mode 'overwrite-mode-binary)
	    (delete-char arg)))
    (while (> arg 0)
      (insert char)
      (setq arg (1- arg)))))

(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (if (eq (char-before (point)) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	;; If the second line started with the fill prefix,
	;; delete the prefix.
	(if (and fill-prefix
		 (<= (+ (point) (length fill-prefix)) (point-max))
		 (string= fill-prefix
			  (buffer-substring (point)
					    (+ (point) (length fill-prefix)))))
	    (delete-region (point) (+ (point) (length fill-prefix))))
	(fixup-whitespace))))

(defun fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'")))
	nil
      (insert ?\ ))))

(defun delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive "*")
  (skip-chars-backward " \t")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun just-one-space ()
  "Delete all spaces and tabs around point, leaving one space."
  (interactive "*")
  (if abbrev-mode ; XEmacs
      (expand-abbrev))
  (skip-chars-backward " \t")
  (if (eq (char-after (point)) ? ) ; XEmacs
      (forward-char 1)
    (insert ? ))
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun delete-blank-lines ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete any immediately following blank lines."
  (interactive "*")
  (let (thisblank singleblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      ;; Set singleblank if there is just one blank line here.
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$")))))))
    ;; Delete preceding blank lines, and this one too if it's the only one.
    (if thisblank
	(progn
	  (beginning-of-line)
	  (if singleblank (forward-line 1))
	  (delete-region (point)
			 (if (re-search-backward "[^ \t\n]" nil t)
			     (progn (forward-line 1) (point))
			   (point-min)))))
    ;; Delete following blank lines, unless the current line is blank
    ;; and there are no following blank lines.
    (if (not (and thisblank singleblank))
	(save-excursion
	  (end-of-line)
	  (forward-line 1)
	  (delete-region (point)
			 (if (re-search-forward "[^ \t\n]" nil t)
			     (progn (beginning-of-line) (point))
			   (point-max)))))
    ;; Handle the special case where point is followed by newline and eob.
    ;; Delete the line, leaving point at eob.
    (if (looking-at "^[ \t]*\n\\'")
	(delete-region (point) (point-max)))))

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  ;; XEmacs change
  (interactive "_")
  (beginning-of-line 1)
  (skip-chars-forward " \t"))

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (newline)
  (indent-according-to-mode))

(defun reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
Indentation of both lines is done according to the current major mode,
which means calling the current value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (newline)
  (indent-according-to-mode))

;; Internal subroutine of delete-char
(defun kill-forward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (+ (point) arg)))

;; Internal subroutine of backward-delete-char
(defun kill-backward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (- (point) arg)))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if a prefix arg was specified."
  (interactive "*p\nP")
  (let ((count arg))
    (save-excursion
      (while (and (> count 0) (not (bobp)))
	(if (eq (char-before (point)) ?\t) ; XEmacs
	    (let ((col (current-column)))
	      (forward-char -1)
	      (setq col (- col (current-column)))
	      (insert-char ?\ col)
	      (delete-char 1)))
	(forward-char -1)
	(setq count (1- count)))))
  (delete-backward-char arg killp)
  ;; XEmacs: In overwrite mode, back over columns while clearing them out,
  ;; unless at end of line.
  (and overwrite-mode (not (eolp))
       (save-excursion (insert-char ?\  arg))))

(defcustom delete-key-deletes-forward t
  "*If non-nil, the DEL key will erase one character forwards.
If nil, the DEL key will erase one character backwards."
  :type 'boolean
  :group 'editing-basics)

(defcustom backward-delete-function 'backward-delete-char
  "*Function called to delete backwards on a delete keypress.
If `delete-key-deletes-forward' is nil, `backward-or-forward-delete-char'
calls this function to erase one character backwards.  Default value
is 'backward-delete-char, with 'backward-delete-char-untabify being a
popular alternate setting."
  :type 'function
  :group 'editing-basics)

;; Trash me, baby.
(defsubst delete-forward-p ()
;  (and delete-key-deletes-forward
;       (or (not (eq (device-type) 'x))
;	   (x-keysym-on-keyboard-sans-modifiers-p 'backspace))))
  delete-key-deletes-forward)

(defun backward-or-forward-delete-char (arg)
  "Delete either one character backwards or one character forwards.
Controlled by the state of `delete-key-deletes-forward' and whether the
BackSpace keysym even exists on your keyboard.  If you don't have a
BackSpace keysym, the delete key should always delete one character
backwards."
  (interactive "*p")
  (if (delete-forward-p)
      (delete-char arg)
    (funcall backward-delete-function arg)))

(defun backward-or-forward-kill-word (arg)
  "Delete either one word backwards or one word forwards.
Controlled by the state of `delete-key-deletes-forward' and whether the
BackSpace keysym even exists on your keyboard.  If you don't have a
BackSpace keysym, the delete key should always delete one character
backwards."
  (interactive "*p")
  (if (delete-forward-p)
      (kill-word arg)
    (backward-kill-word arg)))

(defun backward-or-forward-kill-sentence (arg)
    "Delete either one sentence backwards or one sentence forwards.
Controlled by the state of `delete-key-deletes-forward' and whether the
BackSpace keysym even exists on your keyboard.  If you don't have a
BackSpace keysym, the delete key should always delete one character
backwards."
  (interactive "*P")
  (if (delete-forward-p)
      (kill-sentence arg)
    (backward-kill-sentence (prefix-numeric-value arg))))

(defun backward-or-forward-kill-sexp (arg)
    "Delete either one sexpr backwards or one sexpr forwards.
Controlled by the state of `delete-key-deletes-forward' and whether the
BackSpace keysym even exists on your keyboard.  If you don't have a
BackSpace keysym, the delete key should always delete one character
backwards."
  (interactive "*p")
  (if (delete-forward-p)
      (kill-sexp arg)
    (backward-kill-sexp arg)))

(defun zap-to-char (arg char)
  "Kill up to and including ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (kill-region (point) (with-interactive-search-caps-disable-folding
			   (char-to-string char) nil
			 (search-forward (char-to-string char) nil nil arg)
			 (point))))

(defun zap-up-to-char (arg char)
  "Kill up to ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap up to char: ")
  (kill-region (point) (with-interactive-search-caps-disable-folding
			   (char-to-string char) nil
			 (search-forward (char-to-string char) nil nil arg)
			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
			 (point))))

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the beginning.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (point-min)) is faster and avoids clobbering the mark."
  ;; XEmacs change
  (interactive "_P")
  (push-mark)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (+ 10 (* size (prefix-numeric-value arg))) 10)))
		 (point-min))))
  (if arg (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the end.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  ;; XEmacs change
  (interactive "_P")
  (push-mark)
  ;; XEmacs changes here.
  (let ((scroll-to-end (not (pos-visible-in-window-p (point-max))))
	(size (- (point-max) (point-min))))
    (goto-char (if arg
		   (- (point-max)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (* size (prefix-numeric-value arg)) 10)))
		 (point-max)))
    (cond (arg
           ;; If we went to a place in the middle of the buffer,
           ;; adjust it to the beginning of a line.
           (forward-line 1))
	  ;; XEmacs change
	  (scroll-to-end
           ;; If the end of the buffer is not already on the screen,
           ;; then scroll specially to put it near, but not at, the bottom.
           (recenter -3)))))

;; XEmacs (not in FSF)
(defun mark-beginning-of-buffer (&optional arg)
  "Push a mark at the beginning of the buffer; leave point where it is.
With arg N, push mark N/10 of the way from the true beginning."
  (interactive "P")
  (push-mark (if arg
		 (if (> (buffer-size) 10000)
		     ;; Avoid overflow for large buffer sizes!
		     (* (prefix-numeric-value arg)
			(/ (buffer-size) 10))
		   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
	       (point-min))
             nil
             t))
(define-function 'mark-bob 'mark-beginning-of-buffer)

;; XEmacs (not in FSF)
(defun mark-end-of-buffer (&optional arg)
  "Push a mark at the end of the buffer; leave point where it is.
With arg N, push mark N/10 of the way from the true end."
  (interactive "P")
  (push-mark (if arg
		 (- (1+ (buffer-size))
		    (if (> (buffer-size) 10000)
			;; Avoid overflow for large buffer sizes!
			(* (prefix-numeric-value arg)
			   (/ (buffer-size) 10))
		      (/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
                 (point-max))
             nil
             t))
(define-function 'mark-eob 'mark-end-of-buffer)

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
You probably should not use this function in Lisp programs;
it is usually a mistake for a Lisp function to use any subroutine
that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

;; XEmacs
(defun eval-current-buffer (&optional printflag)
  "Evaluate the current buffer as Lisp code.
Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print."
  (interactive)
  (eval-buffer (current-buffer) printflag))

;; XEmacs
(defun count-words-buffer (&optional buffer)
  "Print the number of words in BUFFER.
If called noninteractively, the value is returned rather than printed.
BUFFER defaults to the current buffer."
  (interactive)
  (let ((words (count-words-region (point-min) (point-max) buffer)))
    (when (interactive-p)
      (message "Buffer has %d words" words))
    words))

;; XEmacs
(defun count-words-region (start end &optional buffer)
  "Print the number of words in region between START and END in BUFFER.
If called noninteractively, the value is returned rather than printed.
BUFFER defaults to the current buffer."
  (interactive "_r")
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (let ((words 0))
      (goto-char start)
      (while (< (point) end)
	(when (forward-word 1)
	  (incf words)))
      (when  (interactive-p)
	(message "Region has %d words" words))
      words)))

(defun count-lines-region (start end)
  "Print number of lines and characters in the region."
  ;; XEmacs change
  (interactive "_r")
  (message "Region has %d lines, %d characters"
	   (count-lines start end) (- end start)))

;; XEmacs
(defun count-lines-buffer (&optional buffer)
  "Print number of lines and characters in BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((cnt (count-lines (point-min) (point-max))))
      (message "Buffer has %d lines, %d characters"
               cnt (- (point-max) (point-min)))
      cnt)))

;;; Modified by Bob Weiner, 8/24/95, to print narrowed line number also.
;;; Expanded by Bob Weiner, BeOpen, on 02/12/1997
(defun what-line ()
  "Print the following variants of the line number of point:
     Region line     - displayed line within the active region
     Collapsed line  - includes only selectively displayed lines;
     Buffer line     - physical line in the buffer;
     Narrowed line   - line number from the start of the buffer narrowing."
  ;; XEmacs change
  (interactive "_")
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(if (region-active-p)
	    (goto-char (region-beginning))
	  (goto-char (point-min)))
	(widen)
	(beginning-of-line)
	(setq start (point))
	(goto-char opoint)
	(beginning-of-line)
	(let* ((buffer-line (1+ (count-lines 1 (point))))
	       (narrowed-p (or (/= start 1)
			       (/= (point-max) (1+ (buffer-size)))))
	       (narrowed-line (if narrowed-p (1+ (count-lines start (point)))))
	       (selective-line (if selective-display
				   (1+ (count-lines start (point) t))))
	       (region-line (if (region-active-p)
				(1+ (count-lines start (point) selective-display)))))
	  (cond (region-line
		 (message "Region line %d; Buffer line %d"
			  region-line buffer-line))
		((and narrowed-p selective-line (/= selective-line narrowed-line))
		 ;; buffer narrowed and some lines selectively displayed
		 (message "Collapsed line %d; Buffer line %d; Narrowed line %d"
			  selective-line buffer-line narrowed-line))
		(narrowed-p
		 ;; buffer narrowed
		 (message "Buffer line %d; Narrowed line %d"
			  buffer-line narrowed-line))
		((and selective-line (/= selective-line buffer-line))
		 ;; some lines selectively displayed
		 (message "Collapsed line %d; Buffer line %d"
			  selective-line buffer-line))
		(t
		 ;; give a basic line count
		 (message "Line %d" buffer-line)))))))
  (setq zmacs-region-stays t))

;;; Bob Weiner, Altrasoft, 02/12/1998
;;; Added the 3rd arg in `count-lines' to conditionalize the counting of
;;; collapsed lines.
(defun count-lines (start end &optional ignore-invisible-lines-flag)
  "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line.

With optional IGNORE-INVISIBLE-LINES-FLAG non-nil, lines collapsed with
selective-display are excluded from the line count."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (and (not ignore-invisible-lines-flag) (eq selective-display t))
	  (save-match-data
	    (let ((done 0))
	      (while (re-search-forward "[\n\C-m]" nil t 40)
		(setq done (+ 40 done)))
	      (while (re-search-forward "[\n\C-m]" nil t 1)
		(setq done (+ 1 done)))
	      (goto-char (point-max))
	      (if (and (/= start end)
		       (not (bolp)))
		  (1+ done)
		done)))
	(- (buffer-size) (forward-line (buffer-size)))))))

(defun what-cursor-position ()
  "Print info on cursor position (on screen and within buffer)."
  ;; XEmacs change
  (interactive "_")
  (let* ((char (char-after (point))) ; XEmacs
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (+ (current-column) (if column-number-start-at-one 1 0))))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d(%d%%) <%d - %d>  column %d %s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d(%d%%)  column %d %s"
		   pos total percent col hscroll))
      ;; XEmacs: don't use single-key-description
      (if (or (/= beg 1) (/= end (1+ total)))
	  (message "Char: %s (0%o, %d, 0x%x)  point=%d of %d(%d%%) <%d - %d>  column %d %s"
		   (text-char-description char) char char char pos total
		   percent beg end col hscroll)
	(message "Char: %s (0%o, %d, 0x%x)  point=%d of %d(%d%%)  column %d %s"
		 (text-char-description char) char char char pos total
		 percent col hscroll)))))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))

;; XEmacs the following are declared elsewhere
;(defvar read-expression-map (cons 'keymap minibuffer-local-map)
;  "Minibuffer keymap used for reading Lisp expressions.")
;(define-key read-expression-map "\M-\t" 'lisp-complete-symbol)

;(put 'eval-expression 'disabled t)

;(defvar read-expression-history nil)

;; We define this, rather than making `eval' interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (expression &optional eval-expression-insert-value)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of the variable `values'.
With prefix argument, insert the result to the current buffer."
  ;(interactive "xEval: ")
  (interactive
   (list (read-from-minibuffer "Eval: "
			       nil read-expression-map t
			       'read-expression-history)
	 current-prefix-arg))
  (setq values (cons (eval expression) values))
  (prin1 (car values)
	 (if eval-expression-insert-value (current-buffer) t)))

;; XEmacs -- extra parameter (variant, but equivalent logic)
(defun edit-and-eval-command (prompt command &optional history)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command (read-expression prompt
				  ;; first try to format the thing readably;
				  ;; and if that fails, print it normally.
				  (condition-case ()
				      (let ((print-readably t))
					(prin1-to-string command))
				    (error (prin1-to-string command)))
				  (or history '(command-history . 1)))))
    (or history (setq history 'command-history))
    (if (consp history)
	(setq history (car history)))
    (if (eq history t)
	nil
      ;; If command was added to the history as a string,
      ;; get rid of that.  We want only evallable expressions there.
      (if (stringp (car (symbol-value history)))
	  (set history (cdr (symbol-value history))))

      ;; If command to be redone does not match front of history,
      ;; add it to the history.
      (or (equal command (car (symbol-value history)))
	  (set history (cons command (symbol-value history)))))
    (eval command)))

(defun repeat-complex-command (arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \\[previous-history-element]
to get different commands to edit and resubmit."
  (interactive "p")
  ;; XEmacs: It looks like our version is better -sb
  (let ((print-level nil))
    (edit-and-eval-command "Redo: "
			   (or (nth (1- arg) command-history)
			       (error ""))
			   (cons 'command-history arg))))

;; XEmacs: Functions moved to minibuf.el
;; previous-matching-history-element
;; next-matching-history-element
;; next-history-element
;; previous-history-element
;; next-complete-history-element
;; previous-complete-history-element

#|
(defun goto-line (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer."
  (interactive "NGoto line: ")
  (setq arg (prefix-numeric-value arg))
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- arg))
      (forward-line (1- arg)))))
|#

;Put this on C-x u, so we can force that rather than C-_ into startup msg
(define-function 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  ;; If we don't get all the way through, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(display-message 'command "Undo!"))
    (or (and (eq last-command 'undo)
	     (eq (current-buffer) last-undo-buffer)) ; XEmacs
	(progn (undo-start)
	       (undo-more 1)))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail buffer-undo-list)
	  done)
      (while (and tail (not done) (not (null (car tail))))
	(if (integerp (car tail))
	    (progn
	      (setq done t)
	      (setq buffer-undo-list (delq (car tail) buffer-undo-list))))
	(setq tail (cdr tail))))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save)))
  ;; If we do get all the way through, make this-command indicate that.
  (setq this-command 'undo))

(defvar pending-undo-list nil
  "Within a run of consecutive undo commands, list remaining to be undone.")

(defvar last-undo-buffer nil)	; XEmacs

(defun undo-start ()
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list buffer-undo-list))

(defun undo-more (count)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call `undo-start' to get ready to undo recent changes,
then call `undo-more' one or more times to undo them."
  (or pending-undo-list
      (error "No further undo information"))
  (setq pending-undo-list (primitive-undo count pending-undo-list)
	last-undo-buffer (current-buffer)))	; XEmacs

;; XEmacs
(defun call-with-transparent-undo (fn &rest args)
  "Apply FN to ARGS, and then undo all changes made by FN to the current
buffer.  The undo records are processed even if FN returns non-locally.
There is no trace of the changes made by FN in the buffer's undo history.

You can use this in a write-file-hooks function with continue-save-buffer
to make the contents of a disk file differ from its in-memory buffer."
  (let ((buffer-undo-list nil)
	;; Kludge to prevent undo list truncation:
	(undo-high-threshold -1)
	(undo-threshold -1)
	(obuffer (current-buffer)))
    (unwind-protect
	(apply fn args)
      ;; Go to the buffer we will restore and make it writable:
      (set-buffer obuffer)
      (save-excursion
	(let ((buffer-read-only nil))
	  (save-restriction
	    (widen)
	    ;; Perform all undos, with further undo logging disabled:
	    (let ((tail buffer-undo-list))
	      (setq buffer-undo-list t)
	      (while tail
		(setq tail (primitive-undo (length tail) tail))))))))))

;; XEmacs: The following are in other files
;; shell-command-history
;; shell-command-switch
;; shell-command
;; shell-command-sentinel


(defconst universal-argument-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-default-binding map 'universal-argument-other-key)
    ;FSFmacs (define-key map [switch-frame] nil)
    (define-key map [(t)] 'universal-argument-other-key)
    (define-key map [(meta t)] 'universal-argument-other-key)
    (define-key map [(control u)] 'universal-argument-more)
    (define-key map [?-] 'universal-argument-minus)
    (define-key map [?0] 'digit-argument)
    (define-key map [?1] 'digit-argument)
    (define-key map [?2] 'digit-argument)
    (define-key map [?3] 'digit-argument)
    (define-key map [?4] 'digit-argument)
    (define-key map [?5] 'digit-argument)
    (define-key map [?6] 'digit-argument)
    (define-key map [?7] 'digit-argument)
    (define-key map [?8] 'digit-argument)
    (define-key map [?9] 'digit-argument)
    map)
  "Keymap used while processing \\[universal-argument].")

(defvar universal-argument-num-events nil
  "Number of argument-specifying events read by `universal-argument'.
`universal-argument-other-key' uses this to discard those events
from (this-command-keys), and reread only the final command.")

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following \\[universal-argument] make up the numeric argument.
\\[universal-argument] following the digits or minus sign ends the argument.
\\[universal-argument] without digits or minus sign provides 4 as argument.
Repeating \\[universal-argument] without digits or minus sign
 multiplies the argument by 4 each time."
  (interactive)
  (setq prefix-arg (list 4))
  (setq zmacs-region-stays t)	; XEmacs
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

;; A subsequent C-u means to multiply the factor by 4 if we've typed
;; nothing but C-u's; otherwise it means to terminate the prefix arg.
(defun universal-argument-more (arg)
  (interactive "_P")			; XEmacs
  (if (consp arg)
      (setq prefix-arg (list (* 4 (car arg))))
    (setq prefix-arg arg)
    (setq overriding-terminal-local-map nil))
  (setq universal-argument-num-events (length (this-command-keys))))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "_P")			; XEmacs
  (cond ((integerp arg)
	  (setq prefix-arg (- arg)))
	 ((eq arg '-)
	  (setq prefix-arg nil))
	 (t
	  (setq prefix-arg '-)))
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

;; XEmacs:  This function not synched with FSF
(defun digit-argument (arg)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "_P")			; XEmacs
  (let* ((event last-command-event)
	 (key (and (key-press-event-p event)
		   (event-key event)))
	 (digit (and key (characterp key) (>= key ?0) (<= key ?9)
		     (- key ?0))))
    (if (null digit)
	(universal-argument-other-key arg)
      (cond ((integerp arg)
	     (setq prefix-arg (+ (* arg 10)
				 (if (< arg 0) (- digit) digit))))
	    ((eq arg '-)
	     ;; Treat -0 as just -, so that -01 will work.
	     (setq prefix-arg (if (zerop digit) '- (- digit))))
	    (t
	     (setq prefix-arg digit)))
      (setq universal-argument-num-events (length (this-command-keys)))
      (setq overriding-terminal-local-map universal-argument-map))))

;; For backward compatibility, minus with no modifiers is an ordinary
;; command if digits have already been entered.
(defun universal-argument-minus (arg)
  (interactive "_P") ; XEmacs
  (if (integerp arg)
      (universal-argument-other-key arg)
    (negative-argument arg)))

;; Anything else terminates the argument and is left in the queue to be
;; executed as a command.
(defun universal-argument-other-key (arg)
  (interactive "_P")			; XEmacs
  (setq prefix-arg arg)
  (let* ((key (this-command-keys))
	 ;; FSF calls silly function `listify-key-sequence' here.
	  (keylist (append key nil)))
    (setq unread-command-events
	   (append (nthcdr universal-argument-num-events keylist)
		   unread-command-events)))
  (reset-this-command-lengths)
  (setq overriding-terminal-local-map nil))


;; XEmacs -- keep zmacs-region active.
(defun forward-to-indentation (arg)
  "Move forward ARG lines and position at first nonblank character."
  (interactive "_p")
  (forward-line arg)
  (skip-chars-forward " \t"))

(defun backward-to-indentation (arg)
  "Move backward ARG lines and position at first nonblank character."
  (interactive "_p")
  (forward-line (- arg))
  (skip-chars-forward " \t"))

(defcustom kill-whole-line nil
  "*Control when and whether `kill-line' removes entire lines.
Note: This only applies when `kill-line' is called interactively;
otherwise, it behaves \"historically\".

If `always', `kill-line' with no arg always kills the whole line,
wherever point is in the line. (If you want to just kill to the end
of the line, use \\[historical-kill-line].)

If not `always' but non-nil, `kill-line' with no arg kills the whole
line if point is at the beginning, and otherwise behaves historically.

If nil, `kill-line' behaves historically."
  :type '(radio (const :tag "Kill to end of line" nil)
		(const :tag "Kill whole line" always)
		(const
		 :tag "Kill whole line at beginning, otherwise end of line" t))
  :group 'killing)

(defun historical-kill-line (&optional arg)
  "Same as `kill-line' but ignores value of `kill-whole-line'."
  (interactive "*P")
  (let ((kill-whole-line nil))
    (if (interactive-p)
	(call-interactively 'kill-line)
      (kill-line arg))))

(defun kill-line (&optional arg)
  "Kill the rest of the current line, or the entire line.
If no nonblanks there, kill thru newline.
If called interactively, may kill the entire line; see `kill-whole-line'.
when given no argument at the beginning of a line.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "*P")
  (kill-region (if (and (interactive-p)
			(not arg)
			(eq kill-whole-line 'always))
		   (save-excursion
		     (beginning-of-line)
		     (point))
		 (point))
	       ;; Don't shift point before doing the delete; that way,
	       ;; undo will record the right position of point.
;; FSF
;	       ;; It is better to move point to the other end of the kill
;	       ;; before killing.  That way, in a read-only buffer, point
;	       ;; moves across the text that is copied to the kill ring.
;	       ;; The choice has no effect on undo now that undo records
;	       ;; the value of point from before the command was run.
;              (progn
	       (save-excursion
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$")
			   (and (interactive-p)
				(or (eq kill-whole-line 'always)
				    (and kill-whole-line (bolp)))))
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

;; XEmacs
(defun backward-kill-line nil
  "Kill back to the beginning of the line."
  (interactive)
  (let ((point (point)))
    (beginning-of-line nil)
    (kill-region (point) point)))


;;;; Window system cut and paste hooks.
;;;
;;; I think that kill-hooks is a better name and more general mechanism
;;; than interprogram-cut-function (from FSFmacs).  I don't like the behavior
;;; of interprogram-paste-function: ^Y should always come from the kill ring,
;;; not the X selection.  But if that were provided, it should be called (and
;;; behave as) yank-hooks instead.  -- jwz

;; [... code snipped ...]

(defcustom kill-hooks nil
  "*Functions run when something is added to the XEmacs kill ring.
These functions are called with one argument, the string most recently
cut or copied.  You can use this to, for example, make the most recent
kill become the X Clipboard selection."
  :type 'hook
  :group 'killing)

;;; `kill-hooks' seems not sufficient because
;;; `interprogram-cut-function' requires more variable about to rotate
;;; the cut buffers.  I'm afraid to change interface of `kill-hooks',
;;; so I add it. (1997-11-03 by MORIOKA Tomohiko)

(defcustom interprogram-cut-function 'own-clipboard
  "Function to call to make a killed region available to other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.
This variable holds a function that Emacs calls whenever text
is put in the kill ring, to make the new kill available to other
programs.

The function takes one or two arguments.
The first argument, TEXT, is a string containing
the text which should be made available.
The second, PUSH, if non-nil means this is a \"new\" kill;
nil means appending to an \"old\" kill."
  :type '(radio (function-item :tag "Send to Clipboard"
			       :format "%t\n"
			       own-clipboard)
		(const :tag "None" nil)
		(function :tag "Other"))
  :group 'killing)

(defcustom interprogram-paste-function 'get-clipboard
  "Function to call to get text cut from other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.
This variable holds a function that Emacs calls to obtain
text that other programs have provided for pasting.

The function should be called with no arguments.  If the function
returns nil, then no other program has provided such text, and the top
of the Emacs kill ring should be used.  If the function returns a
string, that string should be put in the kill ring as the latest kill.

Note that the function should return a string only if a program other
than Emacs has provided a string for pasting; if Emacs provided the
most recent string, the function should return nil.  If it is
difficult to tell whether Emacs or some other program provided the
current string, it is probably good enough to return nil if the string
is equal (according to `string=') to the last text Emacs provided."
  :type '(radio (function-item :tag "Get from Clipboard"
			       :format "%t\n"
			       get-clipboard)
		(const :tag "None" nil)
		(function :tag "Other"))
  :group 'killing)


;;;; The kill ring data structure.

(defvar kill-ring nil
  "List of killed text sequences.
Since the kill ring is supposed to interact nicely with cut-and-paste
facilities offered by window systems, use of this variable should
interact nicely with `interprogram-cut-function' and
`interprogram-paste-function'.  The functions `kill-new',
`kill-append', and `current-kill' are supposed to implement this
interaction; you may want to use them instead of manipulating the kill
ring directly.")

(defcustom kill-ring-max 30
  "*Maximum length of kill ring before oldest elements are thrown away."
  :type 'integer
  :group 'killing)

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defun kill-new (string &optional replace)
  "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
Run `kill-hooks'.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list."
;  (and (fboundp 'menu-bar-update-yank-menu)
;       (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  (if replace
      (setcar kill-ring string)
    (setq kill-ring (cons string kill-ring))
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      (funcall interprogram-cut-function string (not replace)))
  (run-hook-with-args 'kill-hooks string))

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
Run `kill-hooks'."
  (kill-new (if before-p
		(concat string (car kill-ring))
	      (concat (car kill-ring) string)) t))

(defun current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If N is zero, `interprogram-paste-function' is set, and calling it
returns a string, then that string is added to the front of the
kill ring and returned as the latest kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually move the
yanking point\; just return the Nth kill forward."
  (let ((interprogram-paste (and (= n 0)
				 interprogram-paste-function
				 (funcall interprogram-paste-function))))
    (if interprogram-paste
	(progn
	  ;; Disable the interprogram cut function when we add the new
	  ;; text to the kill ring, so Emacs doesn't try to own the
	  ;; selection, with identical text.
	  (let ((interprogram-cut-function nil))
	    (kill-new interprogram-paste))
	  interprogram-paste)
      (or kill-ring (error "Kill ring is empty"))
      (let* ((tem (nthcdr (mod (- n (length kill-ring-yank-pointer))
			       (length kill-ring))
			  kill-ring)))
	(or do-not-move
	    (setq kill-ring-yank-pointer tem))
	(car tem)))))



;;;; Commands for manipulating the kill ring.

;; In FSF killing read-only text just pastes it into kill-ring.  Which
;; is a very bad idea -- see Jamie's comment below.

;(defvar kill-read-only-ok nil
;  "*Non-nil means don't signal an error for killing read-only text.")

(defun kill-region (beg end &optional verbose) ; verbose is XEmacs addition
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character numbers indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring."
  (interactive "*r\np")
;  (interactive
;   (let ((region-hack (and zmacs-regions (eq last-command 'yank))))
;     ;; This lets "^Y^W" work.  I think this is dumb, but zwei did it.
;     (if region-hack (zmacs-activate-region))
;     (prog1
;	 (list (point) (mark) current-prefix-arg)
;       (if region-hack (zmacs-deactivate-region)))))
  ;; beg and end can be markers but the rest of this function is
  ;; written as if they are only integers
  (if (markerp beg) (setq beg (marker-position beg)))
  (if (markerp end) (setq end (marker-position end)))
  (or (and beg end) (if zmacs-regions ;; rewritten for I18N3 snarfing
			(error "The region is not active now")
		      (error "The mark is not set now")))
  (if verbose (if buffer-read-only
		  (lmessage 'command "Copying %d characters"
			    (- (max beg end) (min beg end)))
		(lmessage 'command "Killing %d characters"
			  (- (max beg end) (min beg end)))))
  (cond

   ;; I don't like this large change in behavior -- jwz
   ;; Read-Only text means it shouldn't be deleted, so I'm restoring
   ;; this code, but only for text-properties and not full extents. -sb
   ;; If the buffer is read-only, we should beep, in case the person
   ;; just isn't aware of this.  However, there's no harm in putting
   ;; the region's text in the kill ring, anyway.
   ((or (and buffer-read-only (not inhibit-read-only))
	#| not supported by JEmacs
	(text-property-not-all (min beg end) (max beg end) 'read-only nil)
	|#
	)
   ;; This is redundant.
   ;; (if verbose (message "Copying %d characters"
   ;;			 (- (max beg end) (min beg end))))
    (copy-region-as-kill beg end)
   ;; ;; This should always barf, and give us the correct error.
   ;; (if kill-read-only-ok
   ;;	  (message "Read only text copied to kill ring")
    (setq this-command 'kill-region)
    (barf-if-buffer-read-only)
    (signal 'buffer-read-only (list (current-buffer))))

   ;; In certain cases, we can arrange for the undo list and the kill
   ;; ring to share the same string object.  This code does that.
   ((not (or (eq buffer-undo-list t)
	     (eq last-command 'kill-region)
	     ;; Use = since positions may be numbers or markers.
	     (= beg end)))
    ;; Don't let the undo list be truncated before we can even access it.
    ;; FSF calls this `undo-strong-limit'
    (let ((undo-high-threshold (+ (- end beg) 100))
	  ;(old-list buffer-undo-list)
	  tail)
      (delete-region beg end)
      ;; Search back in buffer-undo-list for this string,
      ;; in case a change hook made property changes.
      (setq tail buffer-undo-list)
      (while (and tail
		  (not (stringp (car-safe (car-safe tail))))) ; XEmacs
	(pop tail))
      ;; Take the same string recorded for undo
      ;; and put it in the kill-ring.
      (and tail
	   (kill-new (car (car tail))))))

   (t
    ;; if undo is not kept, grab the string then delete it (which won't
    ;; add another string to the undo list).
    (copy-region-as-kill beg end)
    (delete-region beg end)))
  (setq this-command 'kill-region))

;; copy-region-as-kill no longer sets this-command, because it's confusing
;; to get two copies of the text when the user accidentally types M-w and
;; then corrects it with the intended C-w.
(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
Run `kill-hooks'."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied."
  (interactive "r")
  (copy-region-as-kill beg end)
  ;; copy before delay, for xclipboard's benefit
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
	    (progn
	      ;; FSF (I'm not sure what this does -sb)
;	      ;; Swap point and mark.
;	      (set-marker (mark-marker) (point) (current-buffer))
	      (goto-char other-end)
              (sit-for 1)
;	      ;; Swap back.
;	      (set-marker (mark-marker) other-end (current-buffer))
              (goto-char opoint)
              ;; If user quit, deactivate the mark
	      ;; as C-g would as a command.
	      (and quit-flag (mark)
                   (zmacs-deactivate-region)))
	  ;; too noisy. -- jwz
;	  (let* ((killed-text (current-kill 0))
;		 (message-len (min (length killed-text) 40)))
;	    (if (= (point) beg)
;		;; Don't say "killed"; that is misleading.
;		(message "Saved text until \"%s\""
;			(substring killed-text (- message-len)))
;	      (message "Saved text from \"%s\""
;		      (substring killed-text 0 message-len))))
	  ))))

(defun append-next-kill ()
  "Cause following command, if it kills, to append to previous kill."
  ;; XEmacs
  (interactive "_")
  (if (interactive-p)
      (progn
	(setq this-command 'kill-region)
	(display-message 'command
	  "If the next command is a kill, it will append"))
    (setq last-command 'kill-region)))

(defun yank-pop (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop'.
At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop' deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let ((inhibit-read-only t)
	(before (< (point) (mark t))))
    (delete-region (point) (mark t))
    ;;(set-marker (mark-marker) (point) (current-buffer))
    (set-mark (point))
    (insert (current-kill arg))
    (if before
	;; This is like exchange-point-and-mark, but doesn't activate the mark.
	;; It is cleaner to avoid activation, even though the command
	;; loop would deactivate the mark because we inserted text.
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker t) (point) (current-buffer))))))
  nil)


(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just C-u as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  ;; If we don't get all the way through, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark (point))
  (insert (current-kill (cond
			 ((listp arg) 0)
			 ((eq arg '-) -1)
			 (t (1- arg)))))
  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      ;; (But it's an unnecessary kludge in XEmacs.)
      ;(goto-char (prog1 (mark t)
		   ;(set-marker (mark-marker) (point) (current-buffer)))))
      (exchange-point-and-mark t))
  ;; If we do get all the way thru, make this-command indicate that.
  (setq this-command 'yank)
  nil)

(defun rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring.
With argument, rotate that many kills forward (or backward, if negative)."
  (interactive "p")
  (current-kill arg))


(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (read-buffer "Insert buffer: "
		   ;; XEmacs: we have different args
		   (other-buffer (current-buffer) nil t)
		   t))))
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark))
  nil)

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   ;; XEmacs: we have different args to other-buffer
   (list (read-buffer "Append to buffer: " (other-buffer (current-buffer)
							 nil t))
	 (region-beginning) (region-end)))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

(defun prepend-to-buffer (buffer start end)
  "Prepend to specified buffer the text of the region.
It is inserted into that buffer after its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BPrepend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun copy-to-buffer (buffer start end)
  "Copy to specified buffer the text of the region.
It is inserted into that buffer, replacing existing text there.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

;FSFmacs
;(put 'mark-inactive 'error-conditions '(mark-inactive error))
;(put 'mark-inactive 'error-message "The mark is not active now")

(defun mark (&optional force buffer)
  "Return this buffer's mark value as integer, or nil if no mark.

If `zmacs-regions' is true, then this returns nil unless the region is
currently in the active (highlighted) state.  With an argument of t, this
returns the mark (if there is one) regardless of the active-region state.
You should *generally* not use the mark unless the region is active, if
the user has expressed a preference for the active-region model.

If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
  (setq buffer (decode-buffer buffer))
;FSFmacs version:
;  (if (or force (not transient-mark-mode) mark-active mark-even-if-inactive)
;      (marker-position (mark-marker))
;    (signal 'mark-inactive nil)))
  (let ((m (mark-marker force buffer)))
    (and m (marker-position m))))

;;;#### FSFmacs
;;; Many places set mark-active directly, and several of them failed to also
;;; run deactivate-mark-hook.  This shorthand should simplify.
;(defsubst deactivate-mark ()
;  "Deactivate the mark by setting `mark-active' to nil.
;\(That makes a difference only in Transient Mark mode.)
;Also runs the hook `deactivate-mark-hook'."
;  (if transient-mark-mode
;      (progn
;	(setq mark-active nil)
;	(run-hooks 'deactivate-mark-hook))))

(defun set-mark (pos &optional buffer)
  "Set this buffer's mark to POS.  Don't use this function!
That is to say, don't use this function unless you want
the user to see that the mark has moved, and you want the previous
mark position to be lost.

Normally, when a new mark is set, the old one should go on the stack.
This is why most applications should use push-mark, not set-mark.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (setq buffer (decode-buffer buffer))
  (set-marker (mark-marker t buffer) pos buffer))
;; FSF
;  (if pos
;     (progn
;	(setq mark-active t)
;	(run-hooks 'activate-mark-hook)
;	(set-marker (mark-marker) pos (current-buffer)))
;    ;; Normally we never clear mark-active except in Transient Mark mode.
;    ;; But when we actually clear out the mark value too,
;    ;; we must clear mark-active in any mode.
;    (setq mark-active nil)
;    (run-hooks 'deactivate-mark-hook)
;    (set-marker (mark-marker) nil)))

(defvar mark-ring nil
  "The list of former marks of the current buffer, most recent first.")
(make-variable-buffer-local 'mark-ring)
(put 'mark-ring 'permanent-local t)

(defcustom mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big."
  :type 'integer
  :group 'killing)

(defvar global-mark-ring nil
  "The list of saved global marks, most recent first.")

(defcustom global-mark-ring-max 16
  "*Maximum size of global mark ring.  \
Start discarding off end if gets this big."
  :type 'integer
  :group 'killing)

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, push old mark position on local mark
ring, and push mark on global mark ring.
With argument, jump to mark, and pop a new position for mark off the ring
\(does not affect global mark ring\).

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (null arg)
      (push-mark nil nil t)
    (if (null (mark t))
	(error "No mark set in this buffer")
      (goto-char (mark t))
      (pop-mark))))

;; XEmacs: Extra parameter
(defun push-mark (&optional location nomsg activate-region buffer)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.
Activate mark if optional third arg ACTIVATE-REGION non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (setq buffer (decode-buffer buffer)) ; XEmacs
  (if (null (mark t buffer)) ; XEmacs
      nil
    ;; The save-excursion / set-buffer is necessary because mark-ring
    ;; is a buffer local variable
    (save-excursion
      (set-buffer buffer)
      (setq mark-ring (cons (copy-marker (mark-marker t buffer)) mark-ring))
      (if (> (length mark-ring) mark-ring-max)
	  (progn
	    (move-marker (car (nthcdr mark-ring-max mark-ring)) nil buffer)
	    (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))))
  (set-mark (or location (point buffer)) buffer)
; (set-marker (mark-marker) (or location (point)) (current-buffer)) ; FSF
  ;; Now push the mark on the global mark ring.
  (if (or (null global-mark-ring)
          (not (eq (marker-buffer (car global-mark-ring)) buffer)))
      ;; The last global mark pushed wasn't in this same buffer.
      (progn
        (setq global-mark-ring (cons (copy-marker (mark-marker t buffer))
                                     global-mark-ring))
        (if (> (length global-mark-ring) global-mark-ring-max)
            (progn
              (move-marker (car (nthcdr global-mark-ring-max global-mark-ring))
                           nil buffer)
              (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (display-message 'command "Mark set"))
  (if activate-region
      (progn
	(setq zmacs-region-stays t)
	(zmacs-activate-region)))
; (if (or activate (not transient-mark-mode)) ; FSF
;     (set-mark (mark t))) ; FSF
  nil)

(defun pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (if mark-ring
      (progn
	(setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker t)))))
	(set-mark (car mark-ring))
	(move-marker (car mark-ring) nil)
	(if (null (mark t)) (ding))
	(setq mark-ring (cdr mark-ring)))))

(define-function 'exchange-dot-and-mark 'exchange-point-and-mark)
(defun exchange-point-and-mark (&optional dont-activate-region)
  "Put the mark where point is now, and point where the mark is now.
The mark is activated unless DONT-ACTIVATE-REGION is non-nil."
  (interactive nil)
(message "exchange-dot-and-mark called dont-act:%s" dont-activate-region)
  (let ((omark (mark t)))
    (if (null omark)
	(error "No mark set in this buffer"))
    (set-mark (point))
    (goto-char omark)
    (or dont-activate-region (zmacs-activate-region)) ; XEmacs
    nil))

;; XEmacs
(defun mark-something (mark-fn movement-fn arg)
  "internal function used by mark-sexp, mark-word, etc."
  (let (newmark (pushp t))
    (save-excursion
      (if (and (eq last-command mark-fn) (mark))
	  ;; Extend the previous state in the same direction:
	  (progn
	    (if (< (mark) (point)) (setq arg (- arg)))
	    (goto-char (mark))
	    (setq pushp nil)))
      (funcall movement-fn arg)
      (setq newmark (point)))
    (if pushp
	(push-mark newmark nil t)
      ;; Do not mess with the mark stack, but merely adjust the previous state:
      (set-mark newmark)
      (activate-region))))

;(defun transient-mark-mode (arg)
;  "Toggle Transient Mark mode.
;With arg, turn Transient Mark mode on if arg is positive, off otherwise.
;
;In Transient Mark mode, when the mark is active, the region is highlighted.
;Changing the buffer \"deactivates\" the mark.
;So do certain other operations that set the mark
;but whose main purpose is something else--for example,
;incremental search, \\[beginning-of-buffer], and \\[end-of-buffer]."
;  (interactive "P")
;  (setq transient-mark-mode
;	(if (null arg)
;	    (not transient-mark-mode)
;	  (> (prefix-numeric-value arg) 0))))

(defun pop-global-mark ()
  "Pop off global mark ring and jump to the top location."
  (interactive)
  ;; Pop entries which refer to non-existent buffers.
  (while (and global-mark-ring (not (marker-buffer (car global-mark-ring))))
    (setq global-mark-ring (cdr global-mark-ring)))
  (or global-mark-ring
      (error "No global mark set"))
  (let* ((marker (car global-mark-ring))
	 (buffer (marker-buffer marker))
	 (position (marker-position marker)))
    (setq global-mark-ring (nconc (cdr global-mark-ring)
				  (list (car global-mark-ring))))
    (set-buffer buffer)
    (or (and (>= position (point-min))
	     (<= position (point-max)))
	(widen))
    (goto-char position)
    (switch-to-buffer buffer)))


(defcustom signal-error-on-buffer-boundary t
  "*Non-nil value causes XEmacs to beep or signal an error when certain interactive commands would move point past (point-min) or (point-max).
The commands that honor this variable are

forward-char-command
backward-char-command
next-line
previous-line
scroll-up-command
scroll-down-command"
  :type 'boolean
  :group 'editing-basics)

;;; After 8 years of waiting ... -sb
(defcustom next-line-add-newlines nil  ; XEmacs
  "*If non-nil, `next-line' inserts newline when the point is at end of buffer.
This behavior used to be the default, and is still default in FSF Emacs.
We think it is an unnecessary and unwanted side-effect."
  :type 'boolean
  :group 'editing-basics)

(defcustom shifted-motion-keys-select-region t
  "*If non-nil, shifted motion keys select text, like in MS Windows.
See also `unshifted-motion-keys-deselect-region'."
  :type 'boolean
  :group 'editing-basics)

(defcustom unshifted-motion-keys-deselect-region t
  "*If non-nil, unshifted motion keys deselect a shifted-motion region.
This only occurs after a region has been selected using shifted motion keys
(not when using the traditional set-mark-then-move method), and has no effect
if `shifted-motion-keys-select-region' is nil."
  :type 'boolean
  :group 'editing-basics)

(defun handle-pre-motion-command-current-command-is-motion ()
  (and (key-press-event-p last-input-event)
  (memq (event-key last-input-event)
	'(left right up down home end prior next
	       kp-left kp-right kp-up kp-down
	       kp-home kp-end kp-prior kp-next))))
  
(defun handle-pre-motion-command ()
  (if
      (and
       (handle-pre-motion-command-current-command-is-motion)
       zmacs-regions
       shifted-motion-keys-select-region
       (not (region-active-p))
       (memq 'shift (event-modifiers last-input-event)))
      (push-mark nil nil t)))

(defun handle-post-motion-command ()
  (if
      (and
       (handle-pre-motion-command-current-command-is-motion)
       zmacs-regions
       (region-active-p))
      (cond ((memq 'shift (event-modifiers last-input-event))
	     (if shifted-motion-keys-select-region
		 (putf this-command-properties 'shifted-motion-command t))
	     (setq zmacs-region-stays t))
	    ((and (getf last-command-properties 'shifted-motion-command)
		  unshifted-motion-keys-deselect-region)
	     (setq zmacs-region-stays nil))
	    (t
	     (setq zmacs-region-stays t)))))

(defun forward-char-command (&optional arg buffer)
  "Move point right ARG characters (left if ARG negative) in BUFFER.
On attempt to pass end of buffer, stop and signal `end-of-buffer'.
On attempt to pass beginning of buffer, stop and signal `beginning-of-buffer'.
Error signaling is suppressed if `signal-error-on-buffer-boundary'
is nil.  If BUFFER is nil, the current buffer is assumed."
  (interactive "_p")
  (if signal-error-on-buffer-boundary
      (forward-char arg buffer)
    (condition-case nil
	(forward-char arg buffer)
      (beginning-of-buffer nil)
      (end-of-buffer nil))))

(defun backward-char-command (&optional arg buffer)
  "Move point left ARG characters (right if ARG negative) in BUFFER.
On attempt to pass end of buffer, stop and signal `end-of-buffer'.
On attempt to pass beginning of buffer, stop and signal `beginning-of-buffer'.
Error signaling is suppressed if `signal-error-on-buffer-boundary'
is nil.  If BUFFER is nil, the current buffer is assumed."
  (interactive "_p")
  (if signal-error-on-buffer-boundary
      (backward-char arg buffer)
    (condition-case nil
	(backward-char arg buffer)
      (beginning-of-buffer nil)
      (end-of-buffer nil))))

(defun scroll-up-command (&optional n)
  "Scroll text of current window upward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
When calling from a program, supply a number as argument or nil.
On attempt to scroll past end of buffer, `end-of-buffer' is signaled.
On attempt to scroll past beginning of buffer, `beginning-of-buffer' is
signaled.

If `signal-error-on-buffer-boundary' is nil, attempts to scroll past buffer
boundaries do not cause an error to be signaled."
  (interactive "_P")
  (if signal-error-on-buffer-boundary
      (scroll-up n)
    (condition-case nil
	(scroll-up n)
      (beginning-of-buffer nil)
      (end-of-buffer nil))))

(defun scroll-down-command (&optional n)
  "Scroll text of current window downward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
When calling from a program, supply a number as argument or nil.
On attempt to scroll past end of buffer, `end-of-buffer' is signaled.
On attempt to scroll past beginning of buffer, `beginning-of-buffer' is
signaled.

If `signal-error-on-buffer-boundary' is nil, attempts to scroll past buffer
boundaries do not cause an error to be signaled."
  (interactive "_P")
  (if signal-error-on-buffer-boundary
      (scroll-down n)
    (condition-case nil
	(scroll-down n)
      (beginning-of-buffer nil)
      (end-of-buffer nil))))

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "_p")
  (if (and next-line-add-newlines (= arg 1))
      (let ((opoint (point)))
	(end-of-line)
	(if (eobp)
	    (newline 1)
	  (goto-char opoint)
	  (line-move arg)))
    (if (interactive-p)
	;; XEmacs:  Not sure what to do about this.  It's inconsistent. -sb
	(condition-case nil
	    (line-move arg)
	  ((beginning-of-buffer end-of-buffer)
	   (when signal-error-on-buffer-boundary
	     (ding nil 'buffer-bound))))
      (line-move arg)))
  nil)

(defun previous-line (arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "_p")
  (if (interactive-p)
      (condition-case nil
	  (line-move (- arg))
	((beginning-of-buffer end-of-buffer)
	 (when signal-error-on-buffer-boundary ; XEmacs
	   (ding nil 'buffer-bound))))
    (line-move (- arg)))
  nil)

(defcustom block-movement-size 6
  "*Number of lines that \"block movement\" commands (\\[forward-block-of-lines], \\[backward-block-of-lines]) move by."
  :type 'integer
  :group 'editing-basics)

(defun backward-block-of-lines ()
  "Move backward by one \"block\" of lines.
The number of lines that make up a block is controlled by
`block-movement-size', which defaults to 6."
  (interactive "_")
  (forward-line (- block-movement-size)))

(defun forward-block-of-lines ()
  "Move forward by one \"block\" of lines.
The number of lines that make up a block is controlled by
`block-movement-size', which defaults to 6."
  (interactive "_")
  (forward-line block-movement-size))

(defcustom track-eol nil
  "*Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.
The beginning of a blank line does not count as the end of a line."
  :type 'boolean
  :group 'editing-basics)

(defcustom goal-column nil
  "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil."
  :type '(choice integer (const :tag "None" nil))
  :group 'editing-basics)
(make-variable-buffer-local 'goal-column)

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was
at the start of current run of vertical motion commands.
When the `track-eol' feature is doing its job, the value is 9999.")
(make-variable-buffer-local 'temporary-goal-column)

;XEmacs: not yet ported, so avoid compiler warnings
;;(eval-when-compile
;;  (defvar inhibit-point-motion-hooks))

(defcustom line-move-ignore-invisible nil
  "*Non-nil means \\[next-line] and \\[previous-line] ignore invisible lines.
Use with care, as it slows down movement significantly.  Outline mode sets this."
  :type 'boolean
  :group 'editing-basics)

;; This is the guts of next-line and previous-line.
;; Arg says how many lines to move.
#|
(defun line-move (arg)
  ;; Don't run any point-motion hooks, and disregard intangibility,
  ;; for intermediate positions.
  (let ((inhibit-point-motion-hooks t)
	(opoint (point))
	new)
    (unwind-protect
	(progn
	  (if (not (or (eq last-command 'next-line)
		       (eq last-command 'previous-line)))
	      (setq temporary-goal-column
		    (if (and track-eol (eolp)
			     ;; Don't count beg of empty line as end of line
			     ;; unless we just did explicit end-of-line.
			     (or (not (bolp)) (eq last-command 'end-of-line)))
			9999
		      (current-column))))
	  (if (and (not (integerp selective-display))
		   (not line-move-ignore-invisible))
	      ;; Use just newline characters.
	      (or (if (> arg 0)
		      (progn (if (> arg 1) (forward-line (1- arg)))
			     ;; This way of moving forward ARG lines
			     ;; verifies that we have a newline after the last one.
			     ;; It doesn't get confused by intangible text.
			     (end-of-line)
			     (zerop (forward-line 1)))
		    (and (zerop (forward-line arg))
			 (bolp)))
		  (signal (if (< arg 0)
			      'beginning-of-buffer
			    'end-of-buffer)
			  nil))
	    ;; Move by arg lines, but ignore invisible ones.
	    (while (> arg 0)
	      (end-of-line)
	      (and (zerop (vertical-motion 1))
		   (signal 'end-of-buffer nil))
	      ;; If the following character is currently invisible,
	      ;; skip all characters with that same `invisible' property value.
	      (while (and (not (eobp))
			  (let ((prop
				 (get-char-property (point) 'invisible)))
			    (if (eq buffer-invisibility-spec t)
				prop
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))))
		(if (get-text-property (point) 'invisible)
		    (goto-char (next-single-property-change (point) 'invisible))
		  (goto-char (next-extent-change (point))))) ; XEmacs
	      (setq arg (1- arg)))
	    (while (< arg 0)
	      (beginning-of-line)
	      (and (zerop (vertical-motion -1))
		   (signal 'beginning-of-buffer nil))
	      (while (and (not (bobp))
			  (let ((prop
				 (get-char-property (1- (point)) 'invisible)))
			    (if (eq buffer-invisibility-spec t)
				prop
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))))
		(if (get-text-property (1- (point)) 'invisible)
		    (goto-char (previous-single-property-change (point) 'invisible))
		  (goto-char (previous-extent-change (point))))) ; XEmacs
	      (setq arg (1+ arg))))
	  (move-to-column (or goal-column temporary-goal-column)))
      ;; Remember where we moved to, go back home,
      ;; then do the motion over again
      ;; in just one step, with intangibility and point-motion hooks
      ;; enabled this time.
      (setq new (point))
      (goto-char opoint)
      (setq inhibit-point-motion-hooks nil)
      (goto-char new)))
  nil)
|#

;;; Many people have said they rarely use this feature, and often type
;;; it by accident.  Maybe it shouldn't even be on a key.
;; It's not on a key, as of 20.2.  So no need for this.
;(put 'set-goal-column 'disabled t)

(defun set-goal-column (arg)
  "Set the current horizontal position as a goal for \\[next-line] and \\[previous-line].
Those commands will move to this position in the line moved to
rather than trying to keep the same horizontal position.
With a non-nil argument, clears out the goal column
so that \\[next-line] and \\[previous-line] resume vertical motion.
The goal column is stored in the variable `goal-column'."
  (interactive "_P") ; XEmacs
  (if arg
      (progn
        (setq goal-column nil)
        (display-message 'command "No goal column"))
    (setq goal-column (current-column))
    (lmessage 'command
	"Goal column %d (use %s with an arg to unset it)"
      goal-column
      (substitute-command-keys "\\[set-goal-column]")))
  nil)

;; deleted FSFmacs terminal randomness hscroll-point-visible stuff.
;; hscroll-step
;; hscroll-point-visible
;; hscroll-window-column
;; right-arrow
;; left-arrow

(defun scroll-other-window-down (lines)
  "Scroll the \"other window\" down.
For more details, see the documentation for `scroll-other-window'."
  (interactive "P")
  (scroll-other-window
   ;; Just invert the argument's meaning.
   ;; We can do that without knowing which window it will be.
   (if (eq lines '-) nil
     (if (null lines) '-
       (- (prefix-numeric-value lines))))))
;(define-key esc-map [?\C-\S-v] 'scroll-other-window-down)

(defun beginning-of-buffer-other-window (arg)
  "Move point to the beginning of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true beginning."
  (interactive "P")
  (let ((orig-window (selected-window))
	(window (other-window-for-scrolling)))
    ;; We use unwind-protect rather than save-window-excursion
    ;; because the latter would preserve the things we want to change.
    (unwind-protect
	(progn
	  (select-window window)
	  ;; Set point and mark in that window's buffer.
	  (beginning-of-buffer arg)
	  ;; Set point accordingly.
	  (recenter '(t)))
      (select-window orig-window))))

(defun end-of-buffer-other-window (arg)
  "Move point to the end of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true end."
  (interactive "P")
  ;; See beginning-of-buffer-other-window for comments.
  (let ((orig-window (selected-window))
	(window (other-window-for-scrolling)))
    (unwind-protect
	(progn
	  (select-window window)
	  (end-of-buffer arg)
	  (recenter '(t)))
      (select-window orig-window))))

(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (eolp) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

;;; A very old implementation of transpose-chars from the old days ...
(defun transpose-preceding-chars (arg)
  "Interchange characters before point.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and not at start of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (not (bolp)) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))


(defun transpose-words (arg)
  "Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-word arg))

(defun transpose-sexps (arg)
  "Like \\[transpose-words] but applies to sexps.
Does not work on a sexp that point is in the middle of
if it is a list or string."
  (interactive "*p")
  (transpose-subr 'forward-sexp arg))

(defun transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (interactive "*p")
  (transpose-subr #'(lambda (arg)
		     (if (= arg 1)
			 (progn
			   ;; Move forward over a line,
			   ;; but create a newline if none exists yet.
			   (end-of-line)
			   (if (eobp)
			       (newline)
			     (forward-char 1)))
		       (forward-line arg)))
		  arg))

;;(eval-when-compile
;;  ;; avoid byte-compiler warnings...
;;  (defvar start1)
;;  (defvar start2)
;;  (defvar end1)
;;  (defvar end2))

; start[12] and end[12] used in transpose-subr-1 below
(defun transpose-subr (mover arg)
  (let (start1 end1 start2 end2)
    (if (= arg 0)
	(progn
	  (save-excursion
	    (funcall mover 1)
	    (setq end2 (point))
	    (funcall mover -1)
	    (setq start2 (point))
	    (goto-char (mark t)) ; XEmacs
	    (funcall mover 1)
	    (setq end1 (point))
	    (funcall mover -1)
	    (setq start1 (point))
	    (transpose-subr-1))
	  (exchange-point-and-mark t))) ; XEmacs
    (while (> arg 0)
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (funcall mover -1)
      (setq start2 (point))
      (transpose-subr-1)
      (goto-char end2)
      (setq arg (1- arg)))
    (while (< arg 0)
      (funcall mover -1)
      (setq start2 (point))
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (transpose-subr-1)
      (setq arg (1+ arg)))))

; start[12] and end[12] used free
(defun transpose-subr-1 ()
  (if (> (min end1 end2) (max start1 start2))
      (error "Don't have two things to transpose"))
  (let ((word1 (buffer-substring start1 end1))
	(word2 (buffer-substring start2 end2)))
    (delete-region start2 end2)
    (goto-char start2)
    (insert word1)
    (goto-char (if (< start1 start2) start1
		 (+ start1 (- (length word1) (length word2)))))
    (delete-char (length word1))
    (insert word2)))

(defcustom comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.
Each mode establishes a different default value for this variable; you
can set the value for a particular mode using that mode's hook."
  :type 'integer
  :group 'fill-comments)
(make-variable-buffer-local 'comment-column)

(defcustom comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'fill-comments)

(defcustom comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'fill-comments)

(defcustom comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line."
  :type 'string
  :group 'fill-comments)

(defconst comment-indent-hook nil
  "Obsolete variable for function to compute desired indentation for a comment.
Use `comment-indent-function' instead.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defconst comment-indent-function
  ;; XEmacs - add at least one space after the end of the text on the
  ;; current line...
  (lambda ()
    (save-excursion
      (beginning-of-line)
      (let ((eol (save-excursion (end-of-line) (point))))
	(and comment-start-skip
	     (re-search-forward comment-start-skip eol t)
	     (setq eol (match-beginning 0)))
	(goto-char eol)
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))))
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defcustom block-comment-start nil
  "*String to insert to start a new comment on a line by itself.
If nil, use `comment-start' instead.
Note that the regular expression `comment-start-skip' should skip this string
as well as the `comment-start' string."
  :type '(choice (const :tag "Use `comment-start'" nil)
		 string)
  :group 'fill-comments)

(defcustom block-comment-end nil
  "*String to insert to end a new comment on a line by itself.
Should be an empty string if comments are terminated by end-of-line.
If nil, use `comment-end' instead."
  :type '(choice (const :tag "Use `comment-end'" nil)
		 string)
  :group 'fill-comments)

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and empty block-comment-start) comment-start))
	 (ender (or (and empty block-comment-end) comment-end)))
    (if (null starter)
	(error "No comment syntax defined")
      (let* ((eolpos (save-excursion (end-of-line) (point)))
	     cpos indent begpos)
	(beginning-of-line)
	(if (re-search-forward comment-start-skip eolpos 'move)
	    (progn (setq cpos (point-marker))
		   ;; Find the start of the comment delimiter.
		   ;; If there were paren-pairs in comment-start-skip,
		   ;; position at the end of the first pair.
		   (if (match-end 1)
		       (goto-char (match-end 1))
		     ;; If comment-start-skip matched a string with
		     ;; internal whitespace (not final whitespace) then
		     ;; the delimiter start at the end of that
		     ;; whitespace.  Otherwise, it starts at the
		     ;; beginning of what was matched.
		     (skip-syntax-backward " " (match-beginning 0))
		     (skip-syntax-backward "^ " (match-beginning 0)))))
	(setq begpos (point))
	;; Compute desired indent.
	(if (= (current-column)
	       (setq indent (funcall comment-indent-function)))
	    (goto-char begpos)
	  ;; If that's different from current, change it.
	  (skip-chars-backward " \t")
	  (delete-region (point) begpos)
	  (indent-to indent))
	;; An existing comment?
	(if cpos
	    (progn (goto-char cpos)
		   (set-marker cpos nil))
	  ;; No, insert one.
	  (insert starter)
	  (save-excursion
	    (insert ender)))))))

(defun set-comment-column (arg)
  "Set the comment column based on point.
With no arg, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (if (eq arg '-)
      (kill-comment nil)
    (if arg
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (re-search-backward comment-start-skip)
	    (beginning-of-line)
	    (re-search-forward comment-start-skip)
	    (goto-char (match-beginning 0))
	    (setq comment-column (current-column))
	    (lmessage 'command "Comment column set to %d" comment-column))
	  (indent-for-comment))
      (setq comment-column (current-column))
      (lmessage 'command "Comment column set to %d" comment-column))))

(defun kill-comment (arg)
  "Kill the comment on this line, if any.
With argument, kill comments on that many lines starting with this one."
  ;; this function loses in a lot of situations.  it incorrectly recognizes
  ;; comment delimiters sometimes (ergo, inside a string), doesn't work
  ;; with multi-line comments, can kill extra whitespace if comment wasn't
  ;; through end-of-line, et cetera.
  (interactive "*P")
  (or comment-start-skip (error "No comment syntax defined"))
  (let ((count (prefix-numeric-value arg)) endc)
    (while (> count 0)
      (save-excursion
	(end-of-line)
	(setq endc (point))
	(beginning-of-line)
	(and (string< "" comment-end)
	     (setq endc
		   (progn
		     (re-search-forward (regexp-quote comment-end) endc 'move)
		     (skip-chars-forward " \t")
		     (point))))
	(beginning-of-line)
	(if (re-search-forward comment-start-skip endc t)
	    (progn
	      (goto-char (match-beginning 0))
	      (skip-chars-backward " \t")
	      (kill-region (point) endc)
	      ;; to catch comments a line beginnings
	      (indent-according-to-mode))))
      (if arg (forward-line 1))
      (setq count (1- count)))))

(defun comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.
With just C-u prefix arg, uncomment each line in region.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
  ;; if someone wants it to only put a comment-start at the beginning and
  ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
  ;; is easy enough.  No option is made here for other than commenting
  ;; every line.
  (interactive "r\nP")
  (or comment-start (error "No comment syntax is defined"))
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (save-restriction
      (let ((cs comment-start) (ce comment-end)
	    numarg)
        (if (consp arg) (setq numarg t)
	  (setq numarg (prefix-numeric-value arg))
	  ;; For positive arg > 1, replicate the comment delims now,
	  ;; then insert the replicated strings just once.
	  (while (> numarg 1)
	    (setq cs (concat cs comment-start)
		  ce (concat ce comment-end))
	    (setq numarg (1- numarg))))
	;; Loop over all lines from BEG to END.
        (narrow-to-region beg end)
        (goto-char beg)
        (while (not (eobp))
          (if (or (eq numarg t) (< numarg 0))
	      (progn
		;; Delete comment start from beginning of line.
		(if (eq numarg t)
		    (while (looking-at (regexp-quote cs))
		      (delete-char (length cs)))
		  (let ((count numarg))
		    (while (and (> 1 (setq count (1+ count)))
				(looking-at (regexp-quote cs)))
		      (delete-char (length cs)))))
		;; Delete comment end from end of line.
                (if (string= "" ce)
		    nil
		  (if (eq numarg t)
		      (progn
			(end-of-line)
			;; This is questionable if comment-end ends in
			;; whitespace.  That is pretty brain-damaged,
			;; though.
			(skip-chars-backward " \t")
			(if (and (>= (- (point) (point-min)) (length ce))
				 (save-excursion
				   (backward-char (length ce))
				   (looking-at (regexp-quote ce))))
			    (delete-char (- (length ce)))))
		    (let ((count numarg))
		      (while (> 1 (setq count (1+ count)))
			(end-of-line)
			;; This is questionable if comment-end ends in
			;; whitespace.  That is pretty brain-damaged though
			(skip-chars-backward " \t")
			(save-excursion
			  (backward-char (length ce))
			  (if (looking-at (regexp-quote ce))
			      (delete-char (length ce))))))))
		(forward-line 1))
	    ;; Insert at beginning and at end.
            (if (looking-at "[ \t]*$") ()
              (insert cs)
              (if (string= "" ce) ()
                (end-of-line)
                (insert ce)))
            (search-forward "\n" nil 'move)))))))

;; XEmacs
(defun prefix-region (prefix)
  "Add a prefix string to each line between mark and point."
  (interactive "sPrefix string: ")
  (if prefix
      (let ((count (count-lines (mark) (point))))
 	(goto-char (min (mark) (point)))
 	(while (> count 0)
          (setq count (1- count))
 	  (beginning-of-line 1)
 	  (insert prefix)
 	  (end-of-line 1)
 	  (forward-char 1)))))


;; XEmacs - extra parameter
(defun backward-word (arg &optional buffer)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call `forward-word' with negative arg."
  (interactive "_p") ; XEmacs
  (forward-word (- arg) buffer))

(defun mark-word (arg)
  "Set mark arg words away from point."
  (interactive "p")
  (mark-something 'mark-word 'forward-word arg))

;; XEmacs modified
(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-region (point) (save-excursion (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p") ; XEmacs
  (kill-word (- arg)))

(defun current-word (&optional strict)
  "Return the word point is on (or a nearby word) as a string.
If optional arg STRICT is non-nil, return nil unless point is within
or adjacent to a word.
If point is not between two word-constituent characters, but immediately
follows one, move back first.
Otherwise, if point precedes a word constituent, move forward first.
Otherwise, move backwards until a word constituent is found and get that word;
if you a newlines is reached first, move forward instead."
  (save-excursion
    (let ((oldpoint (point)) (start (point)) (end (point)))
      (skip-syntax-backward "w_") (setq start (point))
      (goto-char oldpoint)
      (skip-syntax-forward "w_") (setq end (point))
      (if (and (eq start oldpoint) (eq end oldpoint))
	  ;; Point is neither within nor adjacent to a word.
	  (and (not strict)
               (progn
                 ;; Look for preceding word in same line.
                 (skip-syntax-backward "^w_"
                                       (save-excursion
                                         (beginning-of-line) (point)))
                 (if (bolp)
		     ;; No preceding word in same line.
		     ;; Look for following word in same line.
                     (progn
                       (skip-syntax-forward "^w_"
					    (save-excursion
                                              (end-of-line) (point)))
                       (setq start (point))
                       (skip-syntax-forward "w_")
                       (setq end (point)))
                     (setq end (point))
                     (skip-syntax-backward "w_")
                     (setq start (point)))
		 (buffer-substring start end)))
          (buffer-substring start end)))))

(defcustom fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none.
Setting this variable automatically makes it local to the current buffer."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'fill)
(make-variable-buffer-local 'fill-prefix)

(defcustom auto-fill-inhibit-regexp nil
  "*Regexp to match lines which should not be auto-filled."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'fill)

(defvar comment-line-break-function 'indent-new-comment-line
  "*Mode-specific function which line breaks and continues a comment.

This function is only called during auto-filling of a comment section.
The function should take a single optional argument which is a flag
indicating whether soft newlines should be inserted.")

;; defined in mule-base/mule-category.el
(defvar word-across-newline)

;; This function is the auto-fill-function of a buffer
;; when Auto-Fill mode is enabled.
;; It returns t if it really did any work.
;; XEmacs:  This function is totally different.
(defun do-auto-fill ()
  (let (give-up)
    (or (and auto-fill-inhibit-regexp
	     (save-excursion (beginning-of-line)
			     (looking-at auto-fill-inhibit-regexp)))
	(while (and (not give-up) (> (current-column) fill-column))
	  ;; Determine where to split the line.
	  (let ((fill-prefix fill-prefix)
		(fill-point
		 (let ((opoint (point))
		       bounce
		       ;; 97/3/14 jhod: Kinsoku
		       (re-break-point (if (featurep 'mule)
					    (concat "[ \t\n]\\|" word-across-newline
						    ".\\|." word-across-newline)
					"[ \t\n]"))
		       ;; end patch
		       (first t))
		   (save-excursion
		     (move-to-column (1+ fill-column))
		     ;; Move back to a word boundary.
		     (while (or first
				;; If this is after period and a single space,
				;; move back once more--we don't want to break
				;; the line there and make it look like a
				;; sentence end.
				(and (not (bobp))
				     (not bounce)
				     sentence-end-double-space
				     (save-excursion (forward-char -1)
						     (and (looking-at "\\. ")
							  (not (looking-at "\\.  "))))))
		       (setq first nil)
		       ;; 97/3/14 jhod: Kinsoku
		       ; (skip-chars-backward "^ \t\n"))
		       (fill-move-backward-to-break-point re-break-point)
		       ;; end patch
		       ;; If we find nowhere on the line to break it,
		       ;; break after one word.  Set bounce to t
		       ;; so we will not keep going in this while loop.
		       (if (bolp)
			   (progn
			     ;; 97/3/14 jhod: Kinsoku
			     ; (re-search-forward "[ \t]" opoint t)
			     (fill-move-forward-to-break-point re-break-point
							       opoint)
			     ;; end patch
			     (setq bounce t)))
		       (skip-chars-backward " \t"))
		     (if (and (featurep 'mule)
			      (or bounce (bolp))) (kinsoku-process)) ;; 97/3/14 jhod: Kinsoku
		     ;; Let fill-point be set to the place where we end up.
		     (point)))))

	    ;; I'm not sure why Stig made this change but it breaks
	    ;; auto filling in at least C mode so I'm taking it back
	    ;; out.  --cet
	    ;; XEmacs - adaptive fill.
	    ;;(maybe-adapt-fill-prefix
	    ;; (or from (setq from (save-excursion (beginning-of-line)
	    ;;					 (point))))
	    ;; (or to   (setq to (save-excursion (beginning-of-line 2)
	    ;;				       (point))))
	    ;; t)

	    ;; If that place is not the beginning of the line,
	    ;; break the line there.
	    (if (save-excursion
		  (goto-char fill-point)
		  (not (or (bolp) (eolp)))) ; 97/3/14 jhod: during kinsoku processing it is possible to move beyond
		(let ((prev-column (current-column)))
		  ;; If point is at the fill-point, do not `save-excursion'.
		  ;; Otherwise, if a comment prefix or fill-prefix is inserted,
		  ;; point will end up before it rather than after it.
		  (if (save-excursion
			(skip-chars-backward " \t")
			(= (point) fill-point))
		      ;; 1999-09-17 hniksic: turn off Kinsoku until
		      ;; it's debugged.
		      (indent-new-comment-line)
		      ;; 97/3/14 jhod: Kinsoku processing
;		      ;(indent-new-comment-line)
;		      (let ((spacep (memq (char-before (point)) '(?\  ?\t))))
;			(funcall comment-line-break-function)
;			;; if user type space explicitly, leave SPC
;			;; even if there is no WAN.
;			(if spacep
;			    (save-excursion
;			      (goto-char fill-point)
;			      ;; put SPC except that there is SPC
;			      ;; already or there is sentence end.
;			      (or (memq (char-after (point)) '(?\  ?\t))
;				  (fill-end-of-sentence-p)
;				  (insert ?\ )))))
		    (save-excursion
		      (goto-char fill-point)
		      (funcall comment-line-break-function)))
		  ;; If making the new line didn't reduce the hpos of
		  ;; the end of the line, then give up now;
		  ;; trying again will not help.
		  (if (>= (current-column) prev-column)
		      (setq give-up t)))
	      ;; No place to break => stop trying.
	      (setq give-up t)))))))

;; Put FSF one in until I can one or the other working properly, then the
;; other one is history.
;(defun fsf:do-auto-fill ()
;  (let (fc justify
;	   ;; bol
;	   give-up
;	   (fill-prefix fill-prefix))
;    (if (or (not (setq justify (current-justification)))
;	    (null (setq fc (current-fill-column)))
;	    (and (eq justify 'left)
;		 (<= (current-column) fc))
;	    (save-excursion (beginning-of-line)
;			    ;; (setq bol (point))
;			    (and auto-fill-inhibit-regexp
;				 (looking-at auto-fill-inhibit-regexp))))
;	nil ;; Auto-filling not required
;      (if (memq justify '(full center right))
;	  (save-excursion (unjustify-current-line)))

;      ;; Choose a fill-prefix automatically.
;      (if (and adaptive-fill-mode
;	       (or (null fill-prefix) (string= fill-prefix "")))
;	  (let ((prefix
;		 (fill-context-prefix
;		  (save-excursion (backward-paragraph 1) (point))
;		  (save-excursion (forward-paragraph 1) (point))
;		  ;; Don't accept a non-whitespace fill prefix
;		  ;; from the first line of a paragraph.
;		  "^[ \t]*$")))
;	    (and prefix (not (equal prefix ""))
;		 (setq fill-prefix prefix))))

;      (while (and (not give-up) (> (current-column) fc))
;	;; Determine where to split the line.
;	(let ((fill-point
;	       (let ((opoint (point))
;		     bounce
;		     (first t))
;		 (save-excursion
;		   (move-to-column (1+ fc))
;		   ;; Move back to a word boundary.
;		   (while (or first
;			      ;; If this is after period and a single space,
;			      ;; move back once more--we don't want to break
;			      ;; the line there and make it look like a
;			      ;; sentence end.
;			      (and (not (bobp))
;				   (not bounce)
;				   sentence-end-double-space
;				   (save-excursion (forward-char -1)
;						   (and (looking-at "\\. ")
;							(not (looking-at "\\.  "))))))
;		     (setq first nil)
;		     (skip-chars-backward "^ \t\n")
;		     ;; If we find nowhere on the line to break it,
;		     ;; break after one word.  Set bounce to t
;		     ;; so we will not keep going in this while loop.
;		     (if (bolp)
;			 (progn
;			   (re-search-forward "[ \t]" opoint t)
;			   (setq bounce t)))
;		     (skip-chars-backward " \t"))
;		   ;; Let fill-point be set to the place where we end up.
;		   (point)))))
;	  ;; If that place is not the beginning of the line,
;	  ;; break the line there.
;	  (if (save-excursion
;		(goto-char fill-point)
;		(not (bolp)))
;	      (let ((prev-column (current-column)))
;		;; If point is at the fill-point, do not `save-excursion'.
;		;; Otherwise, if a comment prefix or fill-prefix is inserted,
;		;; point will end up before it rather than after it.
;		(if (save-excursion
;		      (skip-chars-backward " \t")
;		      (= (point) fill-point))
;		    (funcall comment-line-break-function t)
;		  (save-excursion
;		    (goto-char fill-point)
;		    (funcall comment-line-break-function t)))
;		;; Now do justification, if required
;		(if (not (eq justify 'left))
;		    (save-excursion
;		      (end-of-line 0)
;		      (justify-current-line justify nil t)))
;		;; If making the new line didn't reduce the hpos of
;		;; the end of the line, then give up now;
;		;; trying again will not help.
;		(if (>= (current-column) prev-column)
;		    (setq give-up t)))
;	    ;; No place to break => stop trying.
;	    (setq give-up t))))
;      ;; Justify last line.
;      (justify-current-line justify t t)
;      t)))

(defvar normal-auto-fill-function 'do-auto-fill
  "The function to use for `auto-fill-function' if Auto Fill mode is turned on.
Some major modes set this.")

(defun auto-fill-mode (&optional arg)
  "Toggle auto-fill mode.
With arg, turn auto-fill mode on if and only if arg is positive.
In Auto-Fill mode, inserting a space at a column beyond `current-fill-column'
automatically breaks the line at a previous space.

The value of `normal-auto-fill-function' specifies the function to use
for `auto-fill-function' when turning Auto Fill mode on."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		       (> (prefix-numeric-value arg) 0))
		   normal-auto-fill-function
		   nil))
    (redraw-modeline)))

;; This holds a document string used to document auto-fill-mode.
(defun auto-fill-function ()
  "Automatically break line at a previous space, in insertion of text."
  nil)

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun set-fill-column (arg)
  "Set `fill-column' to specified argument.
Just \\[universal-argument] as argument means to use the current column
The variable `fill-column' has a separate value for each buffer."
  (interactive "_P") ; XEmacs
  (cond ((integerp arg)
	 (setq fill-column arg))
	((consp arg)
	 (setq fill-column (current-column)))
	;; Disallow missing argument; it's probably a typo for C-x C-f.
	(t
	 (error "set-fill-column requires an explicit argument")))
  (lmessage 'command "fill-column set to %d" fill-column))

(defcustom comment-multi-line t ; XEmacs - this works well with adaptive fill
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.
This is obsolete because you might as well use \\[newline-and-indent]."
  :type 'boolean
  :group 'fill-comments)

(defun indent-new-comment-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
This indents the body of the continued comment
under the previous comment line.

This command is intended for styles where you write a comment per line,
starting a new comment (and terminating it if necessary) on each line.
If you want to continue one comment across several lines, use \\[newline-and-indent].

If a fill column is specified, it overrides the use of the comment column
or comment indentation.

The inserted newline is marked hard if `use-hard-newlines' is true,
unless optional argument SOFT is non-nil."
  (interactive)
  (let (comcol comstart)
    (skip-chars-backward " \t")
    ;; 97/3/14 jhod: Kinsoku processing
    (if (featurep 'mule)
	(kinsoku-process))
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (if soft (insert ?\n) (newline 1))
    (if fill-prefix
	(progn
	  (indent-to-left-margin)
	  (insert fill-prefix))
    ;; #### - Eric Eide reverts to v18 semantics for this function in
    ;; fa-extras, which I'm not gonna do.  His changes are to (1) execute
    ;; the save-excursion below unconditionally, and (2) uncomment the check
    ;; for (not comment-multi-line) further below.  --Stig
      ;;#### jhod: probably need to fix this for kinsoku processing
      (if (not comment-multi-line)
	  (save-excursion
	    (if (and comment-start-skip
		     (let ((opoint (point)))
		       (forward-line -1)
		       (re-search-forward comment-start-skip opoint t)))
		;; The old line is a comment.
		;; Set WIN to the pos of the comment-start.
		;; But if the comment is empty, look at preceding lines
		;; to find one that has a nonempty comment.

		;; If comment-start-skip contains a \(...\) pair,
		;; the real comment delimiter starts at the end of that pair.
		(let ((win (or (match-end 1) (match-beginning 0))))
		  (while (and (eolp) (not (bobp))
			      (let (opoint)
				(beginning-of-line)
				(setq opoint (point))
				(forward-line -1)
				(re-search-forward comment-start-skip opoint t)))
		    (setq win (or (match-end 1) (match-beginning 0))))
		  ;; Indent this line like what we found.
		  (goto-char win)
		  (setq comcol (current-column))
		  (setq comstart
			(buffer-substring (point) (match-end 0)))))))
      (if (and comcol (not fill-prefix))  ; XEmacs - (ENE) from fa-extras.
	  (let ((comment-column comcol)
		(comment-start comstart)
		(block-comment-start comstart)
		(comment-end comment-end))
	    (and comment-end (not (equal comment-end ""))
  ;	       (if (not comment-multi-line)
		     (progn
		       (forward-char -1)
		       (insert comment-end)
		       (forward-char 1))
  ;		 (setq comment-column (+ comment-column (length comment-start))
  ;		       comment-start "")
  ;		   )
		 )
	    (if (not (eolp))
		(setq comment-end ""))
	    (insert ?\n)
	    (forward-char -1)
	    (indent-for-comment)
	    (save-excursion
	      ;; Make sure we delete the newline inserted above.
	      (end-of-line)
	      (delete-char 1)))
	(indent-according-to-mode)))))


(defun set-selective-display (arg)
  "Set `selective-display' to ARG; clear it if no arg.
When the value of `selective-display' is a number > 0,
lines whose indentation is >= that value are not displayed.
The variable `selective-display' has a separate value for each buffer."
  (interactive "P")
  (if (eq selective-display t)
      (error "selective-display already in use for marked lines"))
  (let ((current-vpos
	 (save-restriction
	   (narrow-to-region (point-min) (point))
	   (goto-char (window-start))
	   (vertical-motion (window-height)))))
    (setq selective-display
	  (and arg (prefix-numeric-value arg)))
    (recenter current-vpos))
  (set-window-start (selected-window) (window-start (selected-window)))
  ;; #### doesn't localize properly:
  (princ "selective-display set to " t)
  (prin1 selective-display t)
  (princ "." t))

;; XEmacs
(defun nuke-selective-display ()
  "Ensure that the buffer is not in selective-display mode.
If `selective-display' is t, then restore the buffer text to its original
state before disabling selective display."
  ;; by Stig@hackvan.com
  (interactive)
  (and (eq t selective-display)
       (save-excursion
	 (save-restriction
	   (widen)
	   (goto-char (point-min))
	   (let ((mod-p (buffer-modified-p))
		 (buffer-read-only nil))
	     (while (search-forward "\r" nil t)
	       (delete-char -1)
	       (insert "\n"))
	     (set-buffer-modified-p mod-p)
	     ))))
  (setq selective-display nil))

;;(add-hook 'change-major-mode-hook 'nuke-selective-display)

(defconst overwrite-mode-textual (purecopy " Ovwrt")
  "The string displayed in the mode line when in overwrite mode.")
(defconst overwrite-mode-binary (purecopy " Bin Ovwrt")
  "The string displayed in the mode line when in binary overwrite mode.")

(defun overwrite-mode (arg)
  "Toggle overwrite mode.
With arg, turn overwrite mode on iff arg is positive.
In overwrite mode, printing characters typed in replace existing text
on a one-for-one basis, rather than pushing it to the right.  At the
end of a line, such characters extend the line.  Before a tab,
such characters insert until the tab is filled in.
\\[quoted-insert] still inserts characters in overwrite mode; this
is supposed to make it easier to insert characters when necessary."
  (interactive "P")
  (setq overwrite-mode
	(if (if (null arg) (not overwrite-mode)
	      (> (prefix-numeric-value arg) 0))
	    'overwrite-mode-textual))
  (redraw-modeline))

(defun binary-overwrite-mode (arg)
  "Toggle binary overwrite mode.
With arg, turn binary overwrite mode on iff arg is positive.
In binary overwrite mode, printing characters typed in replace
existing text.  Newlines are not treated specially, so typing at the
end of a line joins the line to the next, with the typed character
between them.  Typing before a tab character simply replaces the tab
with the character typed.
\\[quoted-insert] replaces the text at the cursor, just as ordinary
typing characters do.

Note that binary overwrite mode is not its own minor mode; it is a
specialization of overwrite-mode, entered by setting the
`overwrite-mode' variable to `overwrite-mode-binary'."
  (interactive "P")
  (setq overwrite-mode
	(if (if (null arg)
		(not (eq overwrite-mode 'overwrite-mode-binary))
	      (> (prefix-numeric-value arg) 0))
	    'overwrite-mode-binary))
  (redraw-modeline))

(defcustom line-number-mode nil
  "*Non-nil means display line number in modeline."
  :type 'boolean
  :group 'editing-basics)

(defun line-number-mode (arg)
  "Toggle Line Number mode.
With arg, turn Line Number mode on iff arg is positive.
When Line Number mode is enabled, the line number appears
in the mode line."
  (interactive "P")
  (setq line-number-mode
	(if (null arg) (not line-number-mode)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-modeline))

(defcustom column-number-mode nil
  "*Non-nil means display column number in mode line."
  :type 'boolean
  :group 'editing-basics)

(defun column-number-mode (arg)
  "Toggle Column Number mode.
With arg, turn Column Number mode on iff arg is positive.
When Column Number mode is enabled, the column number appears
in the mode line."
  (interactive "P")
  (setq column-number-mode
	(if (null arg) (not column-number-mode)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-modeline))


(defcustom blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted."
  :type 'boolean
  :group 'paren-blinking)

(defcustom blink-matching-paren-on-screen t
  "*Non-nil means show matching open-paren when it is on screen.
nil means don't show it (but the open-paren can still be shown
when it is off screen."
  :type 'boolean
  :group 'paren-blinking)

(defcustom blink-matching-paren-distance 12000
  "*If non-nil, is maximum distance to search for matching open-paren."
  :type '(choice integer (const nil))
  :group 'paren-blinking)

(defcustom blink-matching-delay 1
  "*The number of seconds that `blink-matching-open' will delay at a match."
  :type 'number
  :group 'paren-blinking)

(defcustom blink-matching-paren-dont-ignore-comments nil
  "*Non-nil means `blink-matching-paren' should not ignore comments."
  :type 'boolean
  :group 'paren-blinking)

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive "_") ; XEmacs
  (and (> (point) (1+ (point-min)))
       blink-matching-paren
       ;; Verify an even number of quoting characters precede the close.
       (= 1 (logand 1 (- (point)
			 (save-excursion
			   (forward-char -1)
			   (skip-syntax-backward "/\\")
			   (point)))))
       (let* ((oldpos (point))
	      (blinkpos)
	      (mismatch))
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region (max (point-min)
					(- (point) blink-matching-paren-distance))
				   oldpos))
	     (condition-case ()
		 (let ((parse-sexp-ignore-comments
			(and parse-sexp-ignore-comments
			     (not blink-matching-paren-dont-ignore-comments))))
		   (setq blinkpos (scan-sexps oldpos -1)))
	       (error nil)))
	   (and blinkpos
		(/= (char-syntax (char-after blinkpos))
		    ?\$)
		(setq mismatch
		      (or (null (matching-paren (char-after blinkpos)))
			  (/= (char-after (1- oldpos))
			      (matching-paren (char-after blinkpos))))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (and blink-matching-paren-on-screen
			 (progn
			   (auto-show-make-point-visible)
			   (sit-for blink-matching-delay)))
		  (goto-char blinkpos)
		  (lmessage 'command "Matches %s"
		    ;; Show what precedes the open in its line, if anything.
		    (if (save-excursion
			  (skip-chars-backward " \t")
			  (not (bolp)))
			(buffer-substring (progn (beginning-of-line) (point))
					  (1+ blinkpos))
		      ;; Show what follows the open in its line, if anything.
		      (if (save-excursion
			    (forward-char 1)
			    (skip-chars-forward " \t")
			    (not (eolp)))
			  (buffer-substring blinkpos
					    (progn (end-of-line) (point)))
			;; Otherwise show the previous nonblank line,
			;; if there is one.
			(if (save-excursion
			      (skip-chars-backward "\n \t")
			      (not (bobp)))
			    (concat
			     (buffer-substring (progn
						 (skip-chars-backward "\n \t")
						 (beginning-of-line)
						 (point))
					       (progn (end-of-line)
						      (skip-chars-backward " \t")
						      (point)))
			     ;; Replace the newline and other whitespace with `...'.
			     "..."
			     (buffer-substring blinkpos (1+ blinkpos)))
			  ;; There is nothing to show except the char itself.
			  (buffer-substring blinkpos (1+ blinkpos))))))))
	     (cond (mismatch
		    (display-message 'no-log "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (display-message 'no-log "Unmatched parenthesis"))))))))

;Turned off because it makes dbx bomb out.
(setq blink-paren-function 'blink-matching-open)

;;(eval-when-compile (defvar myhelp))	; suppress compiler warning

;; XEmacs: Some functions moved to cmdloop.el:
;; keyboard-quit
;; buffer-quit-function
;; keyboard-escape-quit

(defun assoc-ignore-case (key alist)
  "Like `assoc', but assumes KEY is a string and ignores case when comparing."
  (setq key (downcase key))
  (let (element)
    (while (and alist (not element))
      (if (equal key (downcase (car (car alist))))
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))


(defcustom mail-user-agent 'sendmail-user-agent
  "*Your preference for a mail composition package.
Various Emacs Lisp packages (e.g. reporter) require you to compose an
outgoing email message.  This variable lets you specify which
mail-sending package you prefer.

Valid values include:

    sendmail-user-agent -- use the default Emacs Mail package
    mh-e-user-agent     -- use the Emacs interface to the MH mail system
    message-user-agent  -- use the GNUS mail sending package

Additional valid symbols may be available; check with the author of
your package for details."
  :type '(radio (function-item :tag "Default Emacs mail"
			       :format "%t\n"
			       sendmail-user-agent)
		(function-item :tag "Gnus mail sending package"
			       :format "%t\n"
			       message-user-agent)
		(function :tag "Other"))
  :group 'mail)

(defun define-mail-user-agent (symbol composefunc sendfunc
				      &optional abortfunc hookvar)
  "Define a symbol to identify a mail-sending package for `mail-user-agent'.

SYMBOL can be any Lisp symbol.  Its function definition and/or
value as a variable do not matter for this usage; we use only certain
properties on its property list, to encode the rest of the arguments.

COMPOSEFUNC is program callable function that composes an outgoing
mail message buffer.  This function should set up the basics of the
buffer without requiring user interaction.  It should populate the
standard mail headers, leaving the `to:' and `subject:' headers blank
by default.

COMPOSEFUNC should accept several optional arguments--the same
arguments that `compose-mail' takes.  See that function's documentation.

SENDFUNC is the command a user would run to send the message.

Optional ABORTFUNC is the command a user would run to abort the
message.  For mail packages that don't have a separate abort function,
this can be `kill-buffer' (the equivalent of omitting this argument).

Optional HOOKVAR is a hook variable that gets run before the message
is actually sent.  Callers that use the `mail-user-agent' may
install a hook function temporarily on this hook variable.
If HOOKVAR is nil, `mail-send-hook' is used.

The properties used on SYMBOL are `composefunc', `sendfunc',
`abortfunc', and `hookvar'."
  (put symbol 'composefunc composefunc)
  (put symbol 'sendfunc sendfunc)
  (put symbol 'abortfunc (or abortfunc 'kill-buffer))
  (put symbol 'hookvar (or hookvar 'mail-send-hook)))

(define-mail-user-agent 'sendmail-user-agent
  'sendmail-user-agent-compose 'mail-send-and-exit)

(define-mail-user-agent 'message-user-agent
  'message-mail 'message-send-and-exit
  'message-kill-buffer 'message-send-hook)

(defun sendmail-user-agent-compose (&optional to subject other-headers continue
					      switch-function yank-action
					      send-actions)
  (if switch-function
      (let ((special-display-buffer-names nil)
	    (special-display-regexps nil)
	    (same-window-buffer-names nil)
	    (same-window-regexps nil))
	(funcall switch-function "*mail*")))
  (let ((cc (cdr (assoc-ignore-case "cc" other-headers)))
	(in-reply-to (cdr (assoc-ignore-case "in-reply-to" other-headers))))
    (or (mail continue to subject in-reply-to cc yank-action send-actions)
	continue
	(error "Message aborted"))
    (save-excursion
      (goto-char (point-min))
      (search-forward mail-header-separator)
      (beginning-of-line)
      (while other-headers
	(if (not (member (car (car other-headers)) '("in-reply-to" "cc")))
	    (insert (car (car other-headers)) ": "
		    (cdr (car other-headers)) "\n"))
	(setq other-headers (cdr other-headers)))
      t)))

(define-mail-user-agent 'mh-e-user-agent
  'mh-user-agent-compose 'mh-send-letter 'mh-fully-kill-draft
  'mh-before-send-letter-hook)

(defun compose-mail (&optional to subject other-headers continue
			       switch-function yank-action send-actions)
  "Start composing a mail message to send.
This uses the user's chosen mail composition package
as selected with the variable `mail-user-agent'.
The optional arguments TO and SUBJECT specify recipients
and the initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

CONTINUE, if non-nil, says to continue editing a message already
being composed.

SWITCH-FUNCTION, if non-nil, is a function to use to
switch to and display the buffer used for mail composition.

YANK-ACTION, if non-nil, is an action to perform, if and when necessary,
to insert the raw text of the message being replied to.
It has the form (FUNCTION . ARGS).  The user agent will apply
FUNCTION to ARGS, to insert the raw text of the original message.
\(The user agent will also run `mail-citation-hook', *after* the
original text has been inserted in this way.)

SEND-ACTIONS is a list of actions to call when the message is sent.
Each action has the form (FUNCTION . ARGS)."
  (interactive
   (list nil nil nil current-prefix-arg))
  (let ((function (get mail-user-agent 'composefunc)))
    (funcall function to subject other-headers continue
	     switch-function yank-action send-actions)))

(defun compose-mail-other-window (&optional to subject other-headers continue
					    yank-action send-actions)
  "Like \\[compose-mail], but edit the outgoing message in another window."
  (interactive
   (list nil nil nil current-prefix-arg))
  (compose-mail to subject other-headers continue
		'switch-to-buffer-other-window yank-action send-actions))


(defun compose-mail-other-frame (&optional to subject other-headers continue
					    yank-action send-actions)
  "Like \\[compose-mail], but edit the outgoing message in another frame."
  (interactive
   (list nil nil nil current-prefix-arg))
  (compose-mail to subject other-headers continue
		'switch-to-buffer-other-frame yank-action send-actions))


#|
(defun set-variable (var val)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, supply a Lisp expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes.
If VARIABLE is a specifier, VALUE is added to it as an instantiator in
the 'global locale with nil tag set (see `set-specifier').

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value."
  (interactive
   (let* ((var (read-variable "Set variable: "))
	  ;; #### - yucky code replication here.  This should use something
	  ;; from help.el or hyper-apropos.el
	  (minibuffer-help-form
	   '(funcall myhelp))
	  (myhelp
	   #'(lambda ()
	      (with-output-to-temp-buffer "*Help*"
		(prin1 var)
		(princ "\nDocumentation:\n")
		(princ (substring (documentation-property var 'variable-documentation)
				  1))
		(if (boundp var)
		    (let ((print-length 20))
		      (princ "\n\nCurrent value: ")
		      (prin1 (symbol-value var))))
		(save-excursion
		  (set-buffer standard-output)
		  (help-mode))
		nil))))
     (list var
	   (let ((prop (get var 'variable-interactive)))
	     (if prop
		 ;; Use VAR's `variable-interactive' property
		 ;; as an interactive spec for prompting.
		 (call-interactively (list 'lambda '(arg)
					   (list 'interactive prop)
					   'arg))
	       (eval-minibuffer (format "Set %s to value: " var)))))))
  (if (and (boundp var) (specifierp (symbol-value var)))
      (set-specifier (symbol-value var) val)
    (set var val)))
|#

;; XEmacs
(defun activate-region ()
  "Activate the region, if `zmacs-regions' is true.
Setting `zmacs-regions' to true causes LISPM-style active regions to be used.
This function has no effect if `zmacs-regions' is false."
  (interactive)
  (and zmacs-regions (zmacs-activate-region)))

;; XEmacs
(defsubst region-exists-p ()
  "Return t if the region exists.
If active regions are in use (i.e. `zmacs-regions' is true), this means that
 the region is active.  Otherwise, this means that the user has pushed
 a mark in this buffer at some point in the past.
The functions `region-beginning' and `region-end' can be used to find the
 limits of the region."
  (not (null (mark))))

;; XEmacs
(defun region-active-p ()
  "Return non-nil if the region is active.
If `zmacs-regions' is true, this is equivalent to `region-exists-p'.
Otherwise, this function always returns false."
  (and zmacs-regions zmacs-region-extent))

;; A bunch of stuff was moved elsewhere:
;; completion-list-mode-map
;; completion-reference-buffer
;; completion-base-size
;; delete-completion-window
;; previous-completion
;; next-completion
;; choose-completion
;; choose-completion-delete-max-match
;; choose-completion-string
;; completion-list-mode
;; completion-fixup-function
;; completion-setup-function
;; switch-to-completions
;; event stuffs
;; keypad stuffs

;; The rest of this file is not in Lisp in FSF
(defun capitalize-region-or-word (arg)
  "Capitalize the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word arg)))

(defun upcase-region-or-word (arg)
  "Upcase the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word arg)))

(defun downcase-region-or-word (arg)
  "Downcase the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word arg)))

;; #### not localized
(defvar uncapitalized-title-words
  '("the" "a" "an" "in" "of" "for" "to" "and" "but" "at" "on" "as" "by"))

;;(defvar uncapitalized-title-word-regexp
;;  (concat "[ \t]*\\(" (mapconcat #'identity uncapitalized-title-words "\\|")
;;	  "\\)\\>"))

(defun capitalize-string-as-title (string)
  "Capitalize the words in the string, except for small words (as in titles).
The words not capitalized are specified in `uncapitalized-title-words'."
  (let ((buffer (get-buffer-create " *capitalize-string-as-title*")))
    (unwind-protect
	(progn
	  (insert-string string buffer)
	  (capitalize-region-as-title 1 (point-max buffer) buffer)
	  (buffer-string buffer))
      (kill-buffer buffer))))

(defun capitalize-region-as-title (b e &optional buffer)
  "Capitalize the words in the region, except for small words (as in titles).
The words not capitalized are specified in `uncapitalized-title-words'."
  (interactive "r")
  (save-excursion
    (and buffer
	 (set-buffer buffer))
    (save-restriction
      (narrow-to-region b e)
      (goto-char (point-min))
      (let ((first t))
	(while (< (point) (point-max))
	  (if (or first
		  (not (looking-at uncapitalized-title-word-regexp)))
	      (capitalize-word 1)
	    (forward-word 1))
	  (setq first nil))))))

;; Most of the zmacs code is now in elisp.  The only thing left in C
;; are the variables zmacs-regions, zmacs-region-active-p and
;; zmacs-region-stays plus the function zmacs_update_region which
;; simply calls the lisp level zmacs-update-region.  It must remain
;; for convenience, since it is called by core C code.

(defvar zmacs-activate-region-hook nil
  "Function or functions called when the region becomes active;
see the variable `zmacs-regions'.")

(defvar zmacs-deactivate-region-hook nil
  "Function or functions called when the region becomes inactive;
see the variable `zmacs-regions'.")

(defvar zmacs-update-region-hook nil
  "Function or functions called when the active region changes.
This is called after each command that sets `zmacs-region-stays' to t.
See the variable `zmacs-regions'.")

(defvar zmacs-region-extent nil
  "The extent of the zmacs region; don't use this.")

(defvar zmacs-region-rectangular-p nil
  "Whether the zmacs region is a rectangle; don't use this.")

(defun zmacs-make-extent-for-region (region)
  ;; Given a region, this makes an extent in the buffer which holds that
  ;; region, for highlighting purposes.  If the region isn't associated
  ;; with a buffer, this does nothing.
  (let ((buffer nil)
	(valid (and (extentp zmacs-region-extent)
		    (extent-object zmacs-region-extent)
		    (buffer-live-p (extent-object zmacs-region-extent))))
	start end)
    (cond ((consp region)
	   (setq start (min (car region) (cdr region))
		 end (max (car region) (cdr region))
		 valid (and valid
			    (eq (marker-buffer (car region))
				(extent-object zmacs-region-extent)))
		 buffer (marker-buffer (car region))))
	  (t
	   (signal 'error (list "Invalid region" region))))

    (if valid
	nil
      ;; The condition case is in case any of the extents are dead or
      ;; otherwise incapacitated.
      (condition-case ()
	  (if (listp zmacs-region-extent)
	      (mapc 'delete-extent zmacs-region-extent)
	    (delete-extent zmacs-region-extent))
	(error nil)))

    (if valid
	(set-extent-endpoints zmacs-region-extent start end)
      (setq zmacs-region-extent (make-extent start end buffer))

      ;; Make the extent be closed on the right, which means that if
      ;; characters are inserted exactly at the end of the extent, the
      ;; extent will grow to cover them.  This is important for shell
      ;; buffers - suppose one makes a region, and one end is at point-max.
      ;; If the shell produces output, that marker will remain at point-max
      ;; (its position will increase).  So it's important that the extent
      ;; exhibit the same behavior, lest the region covered by the extent
      ;; (the visual indication), and the region between point and mark
      ;; (the actual region value) become different!
      (set-extent-property zmacs-region-extent 'end-open nil)

      ;; use same priority as mouse-highlighting so that conflicts between
      ;; the region extent and a mouse-highlighted extent are resolved by
      ;; the usual size-and-endpoint-comparison method.
      (set-extent-priority zmacs-region-extent mouse-highlight-priority)
      (set-extent-face zmacs-region-extent 'zmacs-region)

      ;; #### It might be better to actually break
      ;; default-mouse-track-next-move-rect out of mouse.el so that we
      ;; can use its logic here.
      (cond
       (zmacs-region-rectangular-p
	(setq zmacs-region-extent (list zmacs-region-extent))
	(default-mouse-track-next-move-rect start end zmacs-region-extent)
	))

      zmacs-region-extent)))

(defun zmacs-region-buffer ()
  "Return the buffer containing the zmacs region, or nil."
  ;; #### this is horrible and kludgy!  This stuff needs to be rethought.
  (and zmacs-regions zmacs-region-active-p
       (or (marker-buffer (mark-marker t))
	   (and (extent-live-p zmacs-region-extent)
	        (buffer-live-p (extent-object zmacs-region-extent))
	        (extent-object zmacs-region-extent)))))

(defun zmacs-activate-region ()
  "Make the region between `point' and `mark' be active (highlighted),
if `zmacs-regions' is true.  Only a very small number of commands
should ever do this.  Calling this function will call the hook
`zmacs-activate-region-hook', if the region was previously inactive.
Calling this function ensures that the region stays active after the
current command terminates, even if `zmacs-region-stays' is not set.
Returns t if the region was activated (i.e. if `zmacs-regions' if t)."
(message "zmacs-activate-region called !z-r:%s" (not zmacs-regions));
  (if (not zmacs-regions)
      nil
    (setq zmacs-region-active-p t
	  zmacs-region-stays t)
    ;;zmacs-region-rectangular-p (and (boundp 'mouse-track-rectangle-p)
    ;; mouse-track-rectangle-p))
(message "zmacs-activate-region 2")
    (if (marker-buffer (mark-marker t))
	(invoke (selected-window) 'activateRegion))
;;	(zmacs-make-extent-for-region (cons (point-marker t) (mark-marker t))))
    (run-hooks 'zmacs-activate-region-hook)
    t))

(defun zmacs-deactivate-region ()
  "Make the region between `point' and `mark' no longer be active,
if `zmacs-regions' is true.  You shouldn't need to call this; the
command loop calls it when appropriate.  Calling this function will
call the hook `zmacs-deactivate-region-hook', if the region was
previously active.  Returns t if the region had been active, nil
otherwise."
  (if (not zmacs-region-active-p)
      nil
    (setq zmacs-region-active-p nil
	  zmacs-region-stays nil
	  zmacs-region-rectangular-p nil)
    (if zmacs-region-extent
	(let ((inhibit-quit t))
	  (if (listp zmacs-region-extent)
	      (mapc 'delete-extent zmacs-region-extent)
	    (delete-extent zmacs-region-extent))
	  (setq zmacs-region-extent nil)))
    (run-hooks 'zmacs-deactivate-region-hook)
    t))

(defun zmacs-update-region ()
  "Update the highlighted region between `point' and `mark'.
You shouldn't need to call this; the command loop calls it
when appropriate.  Calling this function will call the hook
`zmacs-update-region-hook', if the region is active."
  (when zmacs-region-active-p
    (when (marker-buffer (mark-marker t))
      (zmacs-make-extent-for-region (cons (point-marker t)
					  (mark-marker t))))
    (run-hooks 'zmacs-update-region-hook)))

;;;;;;
;;;;;; echo area stuff
;;;;;;

;;; #### Should this be moved to a separate file, for clarity?
;;; -hniksic

;;; The `message-stack' is an alist of labels with messages; the first
;;; message in this list is always in the echo area.  A call to
;;; `display-message' inserts a label/message pair at the head of the
;;; list, and removes any other pairs with that label.  Calling
;;; `clear-message' causes any pair with matching label to be removed,
;;; and this may cause the displayed message to change or vanish.  If
;;; the label arg is nil, the entire message stack is cleared.
;;;
;;; Message/error filtering will be a little tricker to implement than
;;; logging, since messages can be built up incrementally
;;; using clear-message followed by repeated calls to append-message
;;; (this happens with error messages).  For messages which aren't
;;; created this way, filtering could be implemented at display-message
;;; very easily.
;;;
;;; Bits of the logging code are borrowed from log-messages.el by
;;; Robert Potter (rpotter@grip.cis.upenn.edu).

;; need this to terminate the currently-displayed message
;; ("Loading simple ...")
;;(when (and
;;       (not (fboundp 'display-message))
;;       (not (featurep 'debug)))
;;  (send-string-to-terminal "\n"))

(defvar message-stack nil
  "An alist of label/string pairs representing active echo-area messages.
The first element in the list is currently displayed in the echo area.
Do not modify this directly--use the `message' or
`display-message'/`clear-message' functions.")

(defvar remove-message-hook 'log-message
  "A function or list of functions to be called when a message is removed
from the echo area at the bottom of the frame.  The label of the removed
message is passed as the first argument, and the text of the message
as the second argument.")

(defcustom log-message-max-size 50000
  "Maximum size of the \" *Message-Log*\" buffer.  See `log-message'."
  :type 'integer
  :group 'log-message)
;;(make-compatible-variable 'message-log-max 'log-message-max-size)

;; We used to reject quite a lot of stuff here, but it was a bad idea,
;; for two reasons:
;;
;; a) In most circumstances, you *want* to see the message in the log.
;;    The explicitly non-loggable messages should be marked as such by
;;    the issuer.  Gratuitous non-displaying of random regexps made
;;    debugging harder, too (because various reasonable debugging
;;    messages would get eaten).
;;
;; b) It slowed things down.  Yes, visibly.
;;
;; So, I left only a few of the really useless ones on this kill-list.
;;
;;                                            --hniksic
(defcustom log-message-ignore-regexps
  '(;; Note: adding entries to this list slows down messaging
    ;; significantly.  Wherever possible, use message labels.

    ;; Often-seen messages
    "\\`\\'"				; empty message
    "\\`\\(Beginning\\|End\\) of buffer\\'"
    ;;"^Quit$"
    ;; completions
    ;; Many packages print this -- impossible to categorize
    ;;"^Making completion list"
    ;; Gnus
    ;; "^No news is no news$"
    ;; "^No more\\( unread\\)? newsgroups$"
    ;; "^Opening [^ ]+ server\\.\\.\\."
    ;; "^[^:]+: Reading incoming mail"
    ;; "^Getting mail from "
    ;; "^\\(Generating Summary\\|Sorting threads\\|Making sparse threads\\|Scoring\\|Checking new news\\|Expiring articles\\|Sending\\)\\.\\.\\."
    ;; "^\\(Fetching headers for\\|Retrieving newsgroup\\|Reading active file\\)"
    ;; "^No more\\( unread\\)? articles"
    ;; "^Deleting article "
    ;; W3
    ;; "^Parsed [0-9]+ of [0-9]+ ([0-9]+%)"
    )
  "List of regular expressions matching messages which shouldn't be logged.
See `log-message'.

Ideally, packages which generate messages which might need to be ignored
should label them with 'progress, 'prompt, or 'no-log, so they can be
filtered by the log-message-ignore-labels."
  :type '(repeat regexp)
  :group 'log-message)

(defcustom log-message-ignore-labels
  '(help-echo command progress prompt no-log garbage-collecting auto-saving)
  "List of symbols indicating labels of messages which shouldn't be logged.
See `display-message' for some common labels.  See also `log-message'."
  :type '(repeat (symbol :tag "Label"))
  :group 'log-message)

;;Subsumed by view-lossage
;; Not really, I'm adding it back by popular demand. -slb
(defun show-message-log ()
  "Show the \" *Message-Log*\" buffer, which contains old messages and errors."
  (interactive)
  (pop-to-buffer (get-buffer-create " *Message-Log*")))

(defvar log-message-filter-function 'log-message-filter
  "Value must be a function of two arguments: a symbol (label) and
a string (message).  It should return non-nil to indicate a message
should be logged.  Possible values include 'log-message-filter and
'log-message-filter-errors-only.")

(defun log-message-filter (label message)
  "Default value of `log-message-filter-function'.
Messages whose text matches one of the `log-message-ignore-regexps'
or whose label appears in `log-message-ignore-labels' are not saved."
  (let ((r  log-message-ignore-regexps)
	(ok (not (memq label log-message-ignore-labels))))
    (save-match-data
      (while (and r ok)
	(when (string-match (car r) message)
	  (setq ok nil))
	(setq r (cdr r))))
    ok))

(defun log-message-filter-errors-only (label message)
  "For use as the `log-message-filter-function'.  Only logs error messages."
  (eq label 'error))

(defun log-message (label message)
  "Stuff a copy of the message into the \" *Message-Log*\" buffer,
if it satisfies the `log-message-filter-function'.

For use on `remove-message-hook'."
  (when (and (not noninteractive)
	     (funcall log-message-filter-function label message))
    ;; Use save-excursion rather than save-current-buffer because we
    ;; change the value of point.
    (save-excursion
      (set-buffer (get-buffer-create " *Message-Log*"))
      (goto-char (point-max))
      ;(insert (concat (upcase (symbol-name label)) ": "  message "\n"))
      (let (extent)
	;; Mark multiline message with an extent, which `view-lossage'
	;; will recognize.
	(when (string-match "\n" message)
	  (setq extent (make-extent (point) (point)))
	  (set-extent-properties extent '(end-open nil message-multiline t)))
	(insert message "\n")
	(when extent
	  (set-extent-property extent 'end-open t)))
      (when (> (point-max) (max log-message-max-size (point-min)))
	;; Trim log to ~90% of max size.
	(goto-char (max (- (point-max)
			   (truncate (* 0.9 log-message-max-size)))
			(point-min)))
	(forward-line 1)
	(delete-region (point-min) (point))))))

(defun message-displayed-p (&optional return-string frame)
  "Return a non-nil value if a message is presently displayed in the\n\
minibuffer's echo area.  If optional argument RETURN-STRING is non-nil,\n\
return a string containing the message, otherwise just return t."
  ;; by definition, a message is displayed if the echo area buffer is
  ;; non-empty (see also echo_area_active()).  It had better also
  ;; be the case that message-stack is nil exactly when the echo area
  ;; is non-empty.
  (let ((buffer (get-buffer " *Echo Area*")))
    (and (< (point-min buffer) (point-max buffer))
	 (if return-string
	     (buffer-substring nil nil buffer)
	   t))))

;;; Returns the string which remains in the echo area, or nil if none.
;;; If label is nil, the whole message stack is cleared.
(defun clear-message (&optional label frame stdout-p no-restore)
  "Remove any message with the given LABEL from the message-stack,
erasing it from the echo area if it's currently displayed there.
If a message remains at the head of the message-stack and NO-RESTORE
is nil, it will be displayed.  The string which remains in the echo
area will be returned, or nil if the message-stack is now empty.
If LABEL is nil, the entire message-stack is cleared.

Unless you need the return value or you need to specify a label,
you should just use (message nil)."
#|
  (or frame (setq frame (selected-frame)))
  (let ((clear-stream (and message-stack (eq 'stream (frame-type frame)))))
    (remove-message label frame)
(error "hello")
    (let ((inhibit-read-only t)
	  (zmacs-region-stays zmacs-region-stays)) ; preserve from change
      (erase-buffer " *Echo Area*"))
    (if clear-stream
	(send-string-to-terminal ?\n stdout-p))
    (if no-restore
	nil			; just preparing to put another msg up
      (if message-stack
	  (let ((oldmsg (cdr (car message-stack))))
	    (raw-append-message oldmsg frame stdout-p)
	    oldmsg)
	;; #### Should we (redisplay-echo-area) here?  Messes some
	;; things up.
	nil)))
|#
nil)

(defun remove-message (&optional label frame)
  ;; If label is nil, we want to remove all matching messages.
  ;; Must reverse the stack first to log them in the right order.
  (let ((log nil))
    (while (and message-stack
		(or (null label)	; null label means clear whole stack
		    (eq label (car (car message-stack)))))
      (push (car message-stack) log)
      (setq message-stack (cdr message-stack)))
    (let ((s  message-stack))
      (while (cdr s)
	(let ((msg (car (cdr s))))
	  (if (eq label (car msg))
	      (progn
		(push msg log)
		(setcdr s (cdr (cdr s))))
	    (setq s (cdr s))))))
    ;; (possibly) log each removed message
    (while log
      (condition-case e
	  (run-hook-with-args 'remove-message-hook
			      (car (car log)) (cdr (car log)))
	(error (setq remove-message-hook nil)
	       (lwarn 'message-log 'warning
		 "Error caught in `remove-message-hook': %s"
		 (error-message-string e))
	       (let ((inhibit-read-only t))
		 (erase-buffer " *Echo Area*"))
	       (signal (car e) (cdr e))))
      (setq log (cdr log)))))

(defun append-message (label message &optional frame stdout-p)
  (or frame (setq frame (selected-frame)))
  ;; Add a new entry to the message-stack, or modify an existing one
  (let ((top (car message-stack)))
    (if (eq label (car top))
	(setcdr top (concat (cdr top) message))
      (push (cons label message) message-stack)))
  (raw-append-message message frame stdout-p))

;; Really append the message to the echo area.  no fiddling with
;; message-stack.
(defun raw-append-message (message &optional frame stdout-p)
  (unless (equal message "")
    (let ((inhibit-read-only t)
	  (zmacs-region-stays zmacs-region-stays)) ; preserve from change
      (insert-string message " *Echo Area*")
      ;; Conditionalizing on the device type in this way is not that clean,
      ;; but neither is having a device method, as I originally implemented
      ;; it: all non-stream devices behave in the same way.  Perhaps
      ;; the cleanest way is to make the concept of a "redisplayable"
      ;; device, which stream devices are not.  Look into this more if
      ;; we ever create another non-redisplayable device type (e.g.
      ;; processes?  printers?).

      ;; Don't redisplay the echo area if we are executing a macro.
      (if (not executing-kbd-macro)
	  (if (eq 'stream (frame-type frame))
	      (send-string-to-terminal message stdout-p (frame-device frame))
	    (redisplay-echo-area))))))

(defun display-message (label message &optional frame stdout-p)
  "Print a one-line message at the bottom of the frame.  First argument
LABEL is an identifier for this message.  MESSAGE is the string to display.
Use `clear-message' to remove a labelled message.

Here are some standard labels (those marked with `*' are not logged
by default--see the `log-message-ignore-labels' variable):
    message       default label used by the `message' function
    error         default label used for reporting errors
  * progress      progress indicators like \"Converting... 45%\"
  * prompt        prompt-like messages like \"I-search: foo\"
  * command       helper command messages like \"Mark set\"
  * no-log        messages that should never be logged"
  (clear-message label frame stdout-p t)
  (display label) (display ": ") (display message) (display ?\n)
  (force-output))
;;  (append-message label message frame stdout-p))

(defun current-message (&optional frame)
  "Return the current message in the echo area, or nil.
The FRAME argument is currently unused."
  (cdr (car message-stack)))

;;; may eventually be frame-dependent
(defun current-message-label (&optional frame)
  (car (car message-stack)))

(defun message (fmt &rest args)
  "Print a one-line message at the bottom of the frame.
The arguments are the same as to `format'.

If the only argument is nil, clear any existing message; let the
minibuffer contents show."
  ;; questionable junk in the C code
  ;; (if (framep default-minibuffer-frame)
  ;;     (make-frame-visible default-minibuffer-frame))
  (if (and (null fmt) (null args))
      (prog1 nil
	(clear-message nil))
    (let ((str (apply 'format fmt args)))
      (display-message 'message str)
      str)))

(defun lmessage (label fmt &rest args)
  "Print a one-line message at the bottom of the frame.
First argument LABEL is an identifier for this message.  The rest of the
arguments are the same as to `format'.

See `display-message' for a list of standard labels."
  (if (and (null fmt) (null args))
      (prog1 nil
	(clear-message label nil))
    (let ((str (apply 'format fmt args)))
      (display-message label str)
      str)))


;;;;;;
;;;;;; warning stuff
;;;;;;

(defcustom log-warning-minimum-level 'info
  "Minimum level of warnings that should be logged.
The warnings in levels below this are completely ignored, as if they never
happened.

The recognized warning levels, in decreasing order of priority, are
'emergency, 'alert, 'critical, 'error, 'warning, 'notice, 'info, and
'debug.

See also `display-warning-minimum-level'.

You can also control which warnings are displayed on a class-by-class
basis.  See `display-warning-suppressed-classes' and
`log-warning-suppressed-classes'."
  :type '(choice (const emergency) (const alert) (const critical)
		 (const error) (const warning) (const notice)
		 (const info) (const debug))
  :group 'warnings)

(defcustom display-warning-minimum-level 'info
  "Minimum level of warnings that should be displayed.
The warnings in levels below this will be generated, but not
displayed.

The recognized warning levels, in decreasing order of priority, are
'emergency, 'alert, 'critical, 'error, 'warning, 'notice, 'info, and
'debug.

See also `log-warning-minimum-level'.

You can also control which warnings are displayed on a class-by-class
basis.  See `display-warning-suppressed-classes' and
`log-warning-suppressed-classes'."
  :type '(choice (const emergency) (const alert) (const critical)
		 (const error) (const warning) (const notice)
		 (const info) (const debug))
  :group 'warnings)

(defvar log-warning-suppressed-classes nil
  "List of classes of warnings that shouldn't be logged or displayed.
If any of the CLASS symbols associated with a warning is the same as
any of the symbols listed here, the warning will be completely ignored,
as it they never happened.

NOTE: In most circumstances, you should *not* set this variable.
Set `display-warning-suppressed-classes' instead.  That way the suppressed
warnings are not displayed but are still unobtrusively logged.

See also `log-warning-minimum-level' and `display-warning-minimum-level'.")

(defcustom display-warning-suppressed-classes nil
  "List of classes of warnings that shouldn't be displayed.
If any of the CLASS symbols associated with a warning is the same as
any of the symbols listed here, the warning will not be displayed.
The warning will still logged in the *Warnings* buffer (unless also
contained in `log-warning-suppressed-classes'), but the buffer will
not be automatically popped up.

See also `log-warning-minimum-level' and `display-warning-minimum-level'."
  :type '(repeat symbol)
  :group 'warnings)

(defvar warning-count 0
  "Count of the number of warning messages displayed so far.")

(defconst warning-level-alist '((emergency . 8)
				(alert . 7)
				(critical . 6)
				(error . 5)
				(warning . 4)
				(notice . 3)
				(info . 2)
				(debug . 1)))

(defun warning-level-p (level)
  "Non-nil if LEVEL specifies a warning level."
  (and (symbolp level) (assq level warning-level-alist)))

;; If you're interested in rewriting this function, be aware that it
;; could be called at arbitrary points in a Lisp program (when a
;; built-in function wants to issue a warning, it will call out to
;; this function the next time some Lisp code is evaluated).  Therefore,
;; this function *must* not permanently modify any global variables
;; (e.g. the current buffer) except those that specifically apply
;; to the warning system.

(defvar before-init-deferred-warnings nil)

(defun after-init-display-warnings ()
  "Display warnings deferred till after the init file is run.
Warnings that occur before then are deferred so that warning
suppression in the .emacs file will be honored."
  (while before-init-deferred-warnings
    (apply 'display-warning (car before-init-deferred-warnings))
    (setq before-init-deferred-warnings
	  (cdr before-init-deferred-warnings))))

(add-hook 'after-init-hook 'after-init-display-warnings)

(defun display-warning (class message &optional level)
  "Display a warning message.
CLASS should be a symbol describing what sort of warning this is, such
as `resource' or `key-mapping'.  A list of such symbols is also
accepted. (Individual classes can be suppressed; see
`display-warning-suppressed-classes'.)  Optional argument LEVEL can
be used to specify a priority for the warning, other than default priority
`warning'. (See `display-warning-minimum-level').  The message is
inserted into the *Warnings* buffer, which is made visible at appropriate
times."
  (or level (setq level 'warning))
  (or (listp class) (setq class (list class)))
  (check-argument-type 'warning-level-p level)
  (if (and (not (featurep 'infodock))
	   (not init-file-loaded))
      (push (list class message level) before-init-deferred-warnings)
    (catch 'ignored
      (let ((display-p t)
	    (level-num (cdr (assq level warning-level-alist))))
	(if (< level-num (cdr (assq log-warning-minimum-level
				    warning-level-alist)))
	    (throw 'ignored nil))
	(if (intersection class log-warning-suppressed-classes)
	    (throw 'ignored nil))

	(if (< level-num (cdr (assq display-warning-minimum-level
				    warning-level-alist)))
	    (setq display-p nil))
	(if (and display-p
		 (intersection class display-warning-suppressed-classes))
	    (setq display-p nil))
	(let ((buffer (get-buffer-create "*Warnings*")))
	  (when display-p
	    ;; The C code looks at display-warning-tick to determine
	    ;; when it should call `display-warning-buffer'.  Change it
	    ;; to get the C code's attention.
	    (incf display-warning-tick))
	  (with-current-buffer buffer
	    (goto-char (point-max))
	    (incf warning-count)
	    (princ (format "(%d) (%s/%s) "
			   warning-count
			   (mapconcat 'symbol-name class ",")
			   level)
		   buffer)
	    (princ message buffer)
	    (terpri buffer)
	    (terpri buffer)))))))

(defun warn (&rest args)
  "Display a warning message.
The message is constructed by passing all args to `format'.  The message
is placed in the *Warnings* buffer, which will be popped up at the next
redisplay.  The class of the warning is `warning'.  See also
`display-warning'."
  (display-warning 'warning (apply 'format args)))

(defun lwarn (class level &rest args)
  "Display a labeled warning message.
CLASS should be a symbol describing what sort of warning this is, such
as `resource' or `key-mapping'.  A list of such symbols is also
accepted. (Individual classes can be suppressed; see
`display-warning-suppressed-classes'.)  If non-nil, LEVEL can be used
to specify a priority for the warning, other than default priority
`warning'. (See `display-warning-minimum-level').  The message is
inserted into the *Warnings* buffer, which is made visible at appropriate
times.

The rest of the arguments are passed to `format'."
  (display-warning class (apply 'format args)
		   (or level 'warning)))

(defvar warning-marker nil)

;; When this function is called by the C code, all non-local exits are
;; trapped and C-g is inhibited; therefore, it would be a very, very
;; bad idea for this function to get into an infinite loop.

(defun display-warning-buffer ()
  "Make the buffer that contains the warnings be visible.
The C code calls this periodically, right before redisplay."
  (let ((buffer (get-buffer-create "*Warnings*")))
    (when (or (not warning-marker)
	      (not (eq (marker-buffer warning-marker) buffer)))
      (setq warning-marker (make-marker))
      (set-marker warning-marker 1 buffer))
    (if temp-buffer-show-function
        (let ((show-buffer (get-buffer-create "*Warnings-Show*")))
          (save-excursion
            (set-buffer show-buffer)
            (setq buffer-read-only nil)
            (erase-buffer))
          (save-excursion
            (set-buffer buffer)
            (copy-to-buffer show-buffer
                            (marker-position warning-marker)
                            (point-max)))
          (funcall temp-buffer-show-function show-buffer))
      (set-window-start (display-buffer buffer) warning-marker))
    (set-marker warning-marker (point-max buffer) buffer)))

(defun emacs-name ()
  "Return the printable name of this instance of Emacs."
  (cond ((featurep 'infodock) "InfoDock")
	((featurep 'xemacs) "XEmacs")
	(t "Emacs")))
	  
;;; simple.el ends here

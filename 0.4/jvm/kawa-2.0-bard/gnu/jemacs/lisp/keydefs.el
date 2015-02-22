;;; keydefs.el --- Define standard keybindings.

;; Copyright (C) 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; All the global bindings should be here so that one can reload things
;; like files.el without trashing one's personal bindings.

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; This file is dumped with XEmacs

;;; Code:

(defgroup keyboard nil
  "Input from the keyboard."
  :group 'environment)


;;;; created by C code
;;(defvar global-map (current-global-map) "\
;;Default global keymap mapping XEmacs keyboard input into commands.
;;The value is a keymap which is usually (but not necessarily) XEmacs's
;;global map.")

;;;; created by C code
;;(defvar esc-map (symbol-function 'ESC-prefix) "\
;;Default keymap for ESC (meta) commands.
;;The normal global definition of the character ESC indirects to this keymap.")

;;(set-keymap-name global-map 'global-map)
;;(set-keymap-name esc-map 'ESC-prefix)

(define-prefix-command 'Control-X-prefix t)
(defvar ctl-x-map (symbol-function 'Control-X-prefix) "\
Default keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")
(define-key global-map "\C-x" 'Control-X-prefix)

(define-prefix-command 'ctl-x-4-prefix t)
(defvar ctl-x-4-map (symbol-function 'ctl-x-4-prefix) "\
Keymap for subcommands of C-x 4")
(define-key global-map "\C-x4" 'ctl-x-4-prefix)

(define-prefix-command 'ctl-x-5-prefix t)
(defvar ctl-x-5-map (symbol-function 'ctl-x-5-prefix) "\
Keymap for subcommands of C-x 5")
(define-key global-map "\C-x5" 'ctl-x-5-prefix)
#|
(define-prefix-command 'mode-specific-command-prefix t)
(defvar mode-specific-map (symbol-function 'mode-specific-command-prefix) "\
Keymap for characters following C-c.")
(define-key global-map "\C-c" 'mode-specific-command-prefix)

;; FSFmacs buffer.c

(define-key global-map "\C-xb" 'switch-to-buffer)
(define-key global-map "\C-xk" 'kill-buffer)
(define-key global-map "\C-x\C-b" 'list-buffers)
(put 'erase-buffer 'disabled t)         ;from buffer.c

;; FSFmacs casefiddle.c

(define-key global-map "\C-x\C-u" 'upcase-region)
;; This is silly with zmacs regions
;(put 'upcase-region 'disabled t)
(define-key global-map "\C-x\C-l" 'downcase-region)
;; This is silly with zmacs regions
;(put 'downcase-region 'disabled t)
(define-key global-map "\M-u" 'upcase-region-or-word)
(define-key global-map "\M-l" 'downcase-region-or-word)
(define-key global-map "\M-c" 'capitalize-region-or-word)
|#

;; FSFmacs cmds.c

(let ((n 33))
  (while (<= n 255)
    (if (not (= n 127))
        (define-key global-map n 'self-insert-command))
    (setq n (1+ n))))
(define-key global-map " " 'self-insert-command)

(define-key global-map "\C-a" 'beginning-of-line)
(define-key global-map "\C-b" 'backward-char-command)
(define-key global-map "\C-e" 'end-of-line)
(define-key global-map "\C-f" 'forward-char-command)
(define-key global-map "\C-d" 'delete-char)

;; FSFmacs files.el

(define-key global-map "\C-x\C-f" 'find-file)
(define-key global-map "\C-x\C-q" 'toggle-read-only)
(define-key global-map "\C-x\C-r" 'find-file-read-only)
(define-key global-map "\C-x\C-v" 'find-alternate-file)
(define-key global-map "\C-x\C-s" 'save-buffer)
(define-key global-map "\C-xs" 'save-some-buffers)
(define-key global-map "\C-x\C-w" 'write-file)
(define-key global-map "\C-xi" 'insert-file)
(define-key global-map "\M-~" 'not-modified)
(define-key global-map "\C-x\C-d" 'list-directory)
(define-key global-map "\C-x\C-c" 'save-buffers-kill-emacs)

(define-key global-map "\C-x4f" 'find-file-other-window)
(define-key global-map "\C-x4r" 'find-file-read-only-other-window)
(define-key global-map "\C-x4\C-f" 'find-file-other-window)
(define-key global-map "\C-x4b" 'switch-to-buffer-other-window)
(define-key global-map "\C-x4\C-o" 'display-buffer)

(define-key global-map "\C-x5b" 'switch-to-buffer-other-frame)
(define-key global-map "\C-x5f" 'find-file-other-frame)
(define-key global-map "\C-x5\C-f" 'find-file-other-frame)
(define-key global-map "\C-x5r" 'find-file-read-only-other-frame)

;; FSFmacs frame.c
;FSFmacs has these.  It's probably a good idea to provide ways of hooking
;these events, but it's unlikely that it's a good idea to do it this way.
;Just provide a hook, like the existing `select-frame-hook',
;`deselect-frame-hook', `map-frame-hook', and `unmap-frame-hook'.
;#### ergo need hooks for delete-frame and iconify-frame
;(define-key global-map 'switch-frame 'handle-switch-frame)
;(define-key global-map 'delete-frame 'handle-delete-frame)
;(define-key global-map 'iconify-frame 'ignore-event)
;(define-key global-map 'make-frame-visible 'ignore-event)

;; FSFmacs frame.el

;; New FSF19 bindings: C-x 5 as prefix for window commands
(define-key global-map "\C-x52" 'make-frame)
(define-key global-map "\C-x50" 'delete-frame)
(define-key global-map "\C-x5o" 'other-frame)

;; FSFmacs help.el

(define-key global-map (vector help-char) 'help-command)
(define-key global-map 'help 'help-command)
(define-key global-map 'f1 'help-command)

;; FSFmacs indent.el

;;(define-key global-map "\t" 'self-insert-command)
(define-key global-map "\t" 'indent-for-tab-command)
(define-key global-map "\M-\C-\\" 'indent-region)
(define-key global-map "\C-x\t" 'indent-rigidly)
(define-key global-map "\M-i" 'tab-to-tab-stop)
;; XEmacs addition:
(define-key global-map [(shift tab)] 'tab-to-tab-stop)

;; FSFmacs isearch.el

(define-key global-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward)
(define-key global-map "\M-\C-s" 'isearch-forward-regexp)
(define-key global-map "\M-\C-r" 'isearch-backward-regexp)

;; FSFmacs keyboard.c

(define-key global-map "\C-z" 'suspend-emacs-or-iconify-frame)
(define-key global-map "\C-x\C-z" 'suspend-or-iconify-emacs)

;; FSFmacs loaddefs.el

;; New FSF19 bindings: C-x n as a prefix for narrowing commands.
(define-key global-map "\C-xn" (make-sparse-keymap 'narrowing-prefix))
(put 'narrow-to-region 'disabled t)
(define-key global-map "\C-xnn" 'narrow-to-region)
(define-key global-map "\C-xnw" 'widen)
(define-key global-map "\C-xnd" 'narrow-to-defun)
;; Old v18 bindings
;(define-key global-map "\C-xn" 'narrow-to-region)
;(define-key global-map "\C-xw" 'widen)

;;;(define-key global-map "\C-j" 'newline-and-indent)
(define-key global-map "\C-m" 'newline)
(define-key global-map 'return 'newline)
(define-key global-map 'enter 'newline)
(define-key global-map "\C-o" 'open-line)
(define-key global-map "\M-\C-o" 'split-line)
(define-key global-map "\C-q" 'quoted-insert)
(define-key global-map "\M-^" 'delete-indentation)
(define-key global-map "\M-\\" 'delete-horizontal-space)
(define-key global-map "\M-m" 'back-to-indentation)
(define-key global-map "\C-x\C-o" 'delete-blank-lines)
(define-key global-map "\M- " 'just-one-space)
(define-key global-map "\M-z" 'zap-to-char)
(define-key global-map "\M-=" 'count-lines-region)
(define-key global-map "\C-x=" 'what-cursor-position)
(define-key global-map "\M-:" 'eval-expression)
;; Define ESC ESC : like ESC : for people who type ESC ESC out of habit.
(define-key global-map "\M-\e:" 'eval-expression)
;(define-key global-map "\M-\e" 'eval-expression)
;; Do we really need to disable this now that it is harder to type
;; by accident?
;; (put 'eval-expression 'disabled t)
;; Changed from C-x ESC so that function keys work following C-x.
(define-key global-map "\C-x\e\e" 'repeat-complex-command)
;(define-key global-map "\C-x\e" 'repeat-complex-command)
;; From Emacs 20.
(define-key global-map "\C-x\M-:" 'repeat-complex-command)
(define-key global-map "\C-xu" 'advertised-undo)
;; Many people are used to typing C-/ on X terminals and getting C-_.
(define-key global-map '(control /) 'undo)
(define-key global-map "\C-_" 'undo)
(define-key global-map "\M-!" 'shell-command)
(define-key global-map "\M-|" 'shell-command-on-region)

(define-key global-map "\C-u" 'universal-argument)
;; Make Control-0 - Control-9 set the prefix argument, like Meta-0.
(let ((i ?0))
  (while (<= i ?9)
    (define-key global-map (list 'meta i) 'digit-argument)
    (define-key global-map (list 'control i) 'digit-argument)
    (define-key global-map (list 'control 'meta i) 'digit-argument)
    (setq i (1+ i))))
(define-key global-map '(meta -) 'negative-argument)
(define-key global-map '(control -) 'negative-argument)
(define-key global-map '(control meta -) 'negative-argument)

(define-key global-map "\C-k" 'kill-line)
(define-key global-map '(control K) 'historical-kill-line)
(define-key global-map "\C-w" 'kill-region)
(define-key global-map "\M-w" 'kill-ring-save)
(define-key global-map "\M-\C-w" 'append-next-kill)
(define-key global-map "\C-y" 'yank)
(define-key global-map "\M-y" 'yank-pop)

;; Old v18 binding
;(define-key global-map "\C-xa" 'append-to-buffer)

(define-key global-map "\C-@" 'set-mark-command)
;; Many people are used to typing C-SPC and getting C-@.
(define-key global-map '(control ? ) 'set-mark-command)
(define-key global-map "\C-x\C-x" 'exchange-point-and-mark)
(define-key global-map "\C-x\C-@" 'pop-global-mark)
;(define-key global-map [(control x) (control ? )] 'pop-global-mark)

(define-key global-map "\C-n" 'next-line)
(define-key global-map "\C-p" 'previous-line)
;(define-key global-map "\C-x\C-n" 'set-goal-column)
;; XEmacs:
;;; Many people have said they rarely use this feature, and often type
;;; it by accident.  Maybe it shouldn't even be on a key.
;;; Done.  -hniksic
;(put 'set-goal-column 'disabled t)

;;(define-key global-map [menu] 'execute-extended-command)
;;(define-key global-map [find] 'search-forward)

(define-key global-map "\C-t" 'transpose-chars)
(define-key global-map "\M-t" 'transpose-words)
(define-key global-map "\M-\C-t" 'transpose-sexps)
(define-key global-map "\C-x\C-t" 'transpose-lines)

(define-key global-map "\M-;" 'indent-for-comment)
(define-key global-map "\M-j" 'indent-new-comment-line)
(define-key global-map "\M-\C-j" 'indent-new-comment-line)
(define-key global-map "\C-x;" 'set-comment-column)
(define-key global-map "\C-xf" 'set-fill-column)
(define-key global-map "\C-x$" 'set-selective-display)

(define-key global-map "\M-@" 'mark-word)
(define-key global-map "\M-f" 'forward-word)
(define-key global-map "\M-b" 'backward-word)
(define-key global-map "\M-d" 'kill-word)

(define-key global-map "\M-<" 'beginning-of-buffer)
(define-key global-map "\M->" 'end-of-buffer)
(define-key global-map "\C-xh" 'mark-whole-buffer)
(define-key global-map "\M-\\" 'delete-horizontal-space)

(define-key global-map "\M-\C-f" 'forward-sexp)
(define-key global-map "\M-\C-b" 'backward-sexp)
(define-key global-map "\M-\C-u" 'backward-up-list)
(define-key global-map "\M-\C-@" 'mark-sexp)
(define-key global-map "\M-\C-d" 'down-list)
(define-key global-map "\M-\C-k" 'kill-sexp)
(define-key global-map "\M-\C-n" 'forward-list)
(define-key global-map "\M-\C-p" 'backward-list)
(define-key global-map "\M-\C-a" 'beginning-of-defun)
(define-key global-map "\M-\C-e" 'end-of-defun)
(define-key global-map "\M-\C-h" 'mark-defun)
(define-key global-map "\M-\(" 'insert-parentheses)
(define-key global-map "\M-\)" 'move-past-close-and-reindent)
(define-key global-map "\M-\t" 'lisp-complete-symbol)


(define-key global-map "\C-x/" 'point-to-register)
(define-key global-map "\C-xj" 'jump-to-register)
(define-key global-map "\C-xx" 'copy-to-register)
(define-key global-map "\C-xg" 'insert-register)
;; Old v18 binding
;(define-key global-map "\C-xr" 'copy-rectangle-to-register)

;; New FSF19 bindings: C-x r as a prefix for register commands
(define-key global-map "\C-xr" (make-sparse-keymap 'rectangle-prefix))
(define-key global-map "\C-xr\C-@" 'point-to-register)
(define-key global-map "\C-xr " 'point-to-register)
(define-key global-map "\C-xrj" 'jump-to-register)
(define-key global-map "\C-xrs" 'copy-to-register)
(define-key global-map "\C-xrx" 'copy-to-register)
(define-key global-map "\C-xri" 'insert-register)
(define-key global-map "\C-xrg" 'insert-register)
(define-key global-map "\C-xrr" 'copy-rectangle-to-register)
(define-key global-map "\C-xrn" 'number-to-register)
(define-key global-map "\C-xr+" 'increment-register)
(define-key global-map "\C-xrc" 'clear-rectangle)
(define-key global-map "\C-xrk" 'kill-rectangle)
(define-key global-map "\C-xry" 'yank-rectangle)
(define-key global-map "\C-xro" 'open-rectangle)
(define-key global-map "\C-xrt" 'string-rectangle)
(define-key global-map "\C-xrw" 'window-configuration-to-register)
;(define-key global-map "\C-xrf" 'frame-configuration-to-register)

(define-key global-map "\M-q" 'fill-paragraph-or-region)
;(define-key global-map "\M-q" 'fill-paragraph)
;(define-key global-map "\M-g" 'fill-region) ;now bound to goto-line
(define-key global-map "\C-x." 'set-fill-prefix)

; Using {} instead of [] is 1) FSF compatible and 2) allows function
; keys to work on ttys.  M-[ is the beginning of most the function key
; sequences.
(define-key global-map "\M-{" 'backward-paragraph)
(define-key global-map "\M-}" 'forward-paragraph)
(define-key global-map "\M-h" 'mark-paragraph)
(define-key global-map "\M-a" 'backward-sentence)
(define-key global-map "\M-e" 'forward-sentence)
(define-key global-map "\M-k" 'kill-sentence)
;;(define-key global-map "\C-x\177" 'backward-kill-sentence)

(define-key global-map "\C-x[" 'backward-page)
(define-key global-map "\C-x]" 'forward-page)
(define-key global-map "\C-x\C-p" 'mark-page)
(define-key global-map "\C-xl" 'count-lines-page)
(define-key global-map "\C-xnp" 'narrow-to-page)
;; Old v18 bindings
;(define-key global-map "\C-xp" 'narrow-to-page)
(put 'narrow-to-page 'disabled t)

;; Old v18 bindings
;(define-key global-map "\C-x\C-a" 'add-mode-abbrev)
;(define-key global-map "\C-x+" 'add-global-abbrev)
;(define-key global-map "\C-x\C-h" 'inverse-add-mode-abbrev)
;(define-key global-map "\C-x-" 'inverse-add-global-abbrev)

(define-key global-map "\M-'" 'abbrev-prefix-mark)
(define-key global-map "\C-x'" 'expand-abbrev)

;; New FSF19 bindings: C-x a as a prefix for abbrev commands
(define-key global-map "\C-xal" 'add-mode-abbrev)
(define-key global-map "\C-xa\C-a" 'add-mode-abbrev)
(define-key global-map "\C-xag" 'add-global-abbrev)
(define-key global-map "\C-xa+" 'add-mode-abbrev)
(define-key global-map "\C-xaig" 'inverse-add-global-abbrev)
(define-key global-map "\C-xail" 'inverse-add-mode-abbrev)
(define-key global-map "\C-xa-" 'inverse-add-global-abbrev)
(define-key global-map "\C-xae" 'expand-abbrev)
(define-key global-map "\C-xa'" 'expand-abbrev)

(define-key global-map "\M-\C-l" 'switch-to-other-buffer)

;; Default binding of "Backspace" is no longer the same as delete.
;; Default binding of "Control-h" is help.
(define-key global-map 'backspace 'delete-backward-char)
(define-key global-map '(meta backspace) 'backward-kill-word)

(define-key global-map "\M-\C-z" 'activate-region)

;; FSFmacs macros.c

(define-key global-map "\C-xe" 'call-last-kbd-macro)
(define-key global-map "\C-x\(" 'start-kbd-macro)
(define-key global-map "\C-x\)" 'end-kbd-macro)

;; FSFmacs macros.el

(define-key global-map "\C-xq" 'kbd-macro-query)


;; FSFmacs minibuffer.c
; see also minibuf.el

(define-key global-map "\M-\C-c" 'exit-recursive-edit)
(define-key global-map "\C-]" 'abort-recursive-edit)
(define-key global-map "\M-x" 'execute-extended-command)

;; FSFmacs window.c

(define-key global-map "\C-x0" 'delete-window)
(define-key global-map "\C-x1" 'delete-other-windows)
(define-key global-map "\C-x2" 'split-window-vertically)
(define-key global-map "\C-x3" 'split-window-horizontally)
;; Old XEmacs binding
;;(define-key global-map "\C-x5" 'split-window-horizontally)
(define-key global-map "\C-xo" 'other-window)
(define-key global-map "\C-x^" 'enlarge-window)
(define-key global-map "\C-x<" 'scroll-left)
(define-key global-map "\C-x>" 'scroll-right)

(define-key global-map "\C-v" 'scroll-up-command)
(define-key global-map "\M-v" 'scroll-down-command)
(define-key global-map "\M-\C-v" 'scroll-other-window)
; meta-shift-V, that is.
(define-key global-map '(meta V) 'scroll-other-window-down)

(define-key global-map "\C-l" 'recenter)
(define-key global-map "\M-r" 'move-to-window-line)

;; FSFmacs window.el

(define-key global-map "\C-x6" 'window-configuration-to-register)
;(define-key global-map "\C-x7" 'jump-to-register);ie register-to-window-config
(define-key global-map "\C-x}" 'enlarge-window-horizontally)
(define-key global-map "\C-x{" 'shrink-window-horizontally)
;; New FSF19 bindings
(define-key global-map "\C-x-" 'shrink-window-if-larger-than-buffer)
(define-key global-map "\C-x+" 'balance-windows)
(define-key ctl-x-4-map "0" 'kill-buffer-and-window)

(define-key global-map "\C-g" 'keyboard-quit)
(let ((ch (quit-char)))
  (if (or (characterp ch) (integerp ch))
      (setq ch (char-to-string ch)))
  (define-key global-map ch 'keyboard-quit))
(define-key global-map "\e\e\e" 'keyboard-escape-quit)



(define-key global-map "\M-%" 'query-replace)

;; FSF v20 binding
(define-key global-map [(control meta %)] 'query-replace-regexp)


; autoloaded
;(define-key global-map "\C-x4a" 'add-change-log-entry-other-window)

; autoloaded
;(define-key global-map "\C-x`" 'next-error)

; autoloaded
;(define-key global-map "\M-/" 'dabbrev-expand)

; autoloaded
;(define-key global-map "\C-xd" 'dired)

; autoloaded
;(define-key global-map "\C-x4d" 'dired-other-window)

(define-key global-map "\M-$" 'ispell-word)

(define-key global-map "\C-xm" 'compose-mail)
(define-key global-map "\C-x4m" 'compose-mail-other-window)
(define-key global-map "\C-x5m" 'compose-mail-other-frame)

(define-key global-map "\M-." 'find-tag)

(define-key global-map "\C-x4." 'find-tag-other-window)

(define-key global-map "\M-," 'tags-loop-continue)


(define-key global-map '(control <) 'mark-beginning-of-buffer)
(define-key global-map '(control >) 'mark-end-of-buffer)

(define-key global-map "\C-x\C-e" 'eval-last-sexp) ;bogus!


(define-key global-map "\M-g" 'goto-line)

;; Keypad type things

;; I removed all the fkey crap, because where-is is now smart enough
;; to show all bindings. --ben

;;; These aren't bound to kbd macros like "\C-b" so that they have the
;; expected behavior even in, for example, vi-mode.

;; We use here symbolic names, assuming that the corresponding keys will
;; generate these keysyms.  This is not true on Suns, but x-win-sun.el 
;; fixes that.  If it turns out that the semantics of these keys should
;; differ from server to server, this should be moved into server-specific
;; files, but these appear to be the standard Motif and PC bindings.

;; movement by units
(define-key global-map 'left		'backward-char-command)
(define-key global-map 'right		'forward-char-command)
(define-key global-map 'up		'previous-line)
(define-key global-map 'down		'next-line)

(define-key global-map 'kp-left		'backward-char-command)
(define-key global-map 'kp-right	'forward-char-command)
(define-key global-map 'kp-up		'previous-line)
(define-key global-map 'kp-down		'next-line)
#|
;; movement by pages
(define-key global-map 'prior		'scroll-down-command)
(define-key global-map 'next		'scroll-up-command)
(define-key global-map '(control prior)	'scroll-right)
(define-key global-map '(control next)	'scroll-left)
(define-key global-map 'kp-prior	'scroll-down-command)
(define-key global-map 'kp-next		'scroll-up-command)
(define-key global-map '(control kp-prior) 'scroll-right)
(define-key global-map '(control kp-next) 'scroll-left)

;; movement to the limits
(define-key global-map 'home		'beginning-of-line)
(define-key global-map 'end		'end-of-line)
(define-key global-map '(control home)	'beginning-of-buffer)
(define-key global-map '(control end)	'end-of-buffer)
(define-key global-map 'kp-home		'beginning-of-line)
(define-key global-map 'kp-end		'end-of-line)
(define-key global-map '(control kp-home) 'beginning-of-buffer)
(define-key global-map '(control kp-end) 'end-of-buffer)

;; on which systems do these exist?
(define-key global-map 'begin		'beginning-of-line)
(define-key global-map '(control begin)	'beginning-of-buffer)

;; movement by larger blocks
(define-key global-map '(control left)	'backward-word)
(define-key global-map '(control right)	'forward-word)
(define-key global-map '(control up)	'backward-block-of-lines)
(define-key global-map '(control down)	'forward-block-of-lines)
(define-key global-map '(control kp-left) 'backward-word)
(define-key global-map '(control kp-right) 'forward-word)
(define-key global-map '(control kp-up)	'backward-block-of-lines)
(define-key global-map '(control kp-down) 'forward-block-of-lines)

;; context-sensitive movement
;; (meta control left/right) should be reserved for bindings that
;; switch between buffers/web pages/etc.
(define-key global-map '(meta left)	'backward-sexp)
(define-key global-map '(meta right)	'forward-sexp)
(define-key global-map '(meta up)	'backward-sentence)
(define-key global-map '(meta down)	'forward-sentence)
(define-key global-map '(meta control up) 'backward-paragraph)
(define-key global-map '(meta control down) 'forward-paragraph)
(define-key global-map '(meta control home)	'beginning-of-defun)
(define-key global-map '(meta control end)	'end-of-defun)
(define-key global-map '(meta control prior)	'backward-page)
(define-key global-map '(meta control next)	'forward-page)
(define-key global-map '(meta kp-left)  'backward-sexp)
(define-key global-map '(meta kp-right) 'forward-sexp)
(define-key global-map '(meta kp-up)	'backward-sentence)
(define-key global-map '(meta kp-down)	'forward-sentence)
(define-key global-map '(meta control kp-up) 'backward-paragraph)
(define-key global-map '(meta control kp-down) 'forward-paragraph)
(define-key global-map '(meta control kp-home)	'beginning-of-defun)
(define-key global-map '(meta control kp-end)	'end-of-defun)
(define-key global-map '(meta control kp-prior)	'backward-page)
(define-key global-map '(meta control kp-next)	'forward-page)

;; movement between windows
(define-key global-map '(control tab)	'other-window)
(define-key global-map '(control shift tab) 'backward-other-window)

;; movement in other windows
(define-key global-map '(meta next)	'scroll-other-window)
(define-key global-map '(meta prior)	'scroll-other-window-down)
(define-key global-map '(meta home)	'beginning-of-buffer-other-window)
(define-key global-map '(meta end)	'end-of-buffer-other-window)
(define-key global-map '(meta kp-next)	'scroll-other-window)
(define-key global-map '(meta kp-prior)	'scroll-other-window-down)
(define-key global-map '(meta kp-home)	'beginning-of-buffer-other-window)
(define-key global-map '(meta kp-end)	'end-of-buffer-other-window)
|#
;; the infamous delete key
(define-key global-map 'delete	        'backward-or-forward-delete-char)
#|
(define-key global-map '(meta delete)	'backward-or-forward-kill-word)
(define-key global-map [(control x) (delete)]
				        'backward-or-forward-kill-sentence)
(define-key global-map 'kp-delete	'backward-or-forward-delete-char)
(define-key global-map '(meta kp-delete) 'backward-or-forward-kill-word)
(define-key global-map [(control x) (kp-delete)]
					'backward-or-forward-kill-sentence)

;; don't try this one at home, kids.
(define-key global-map '(control meta delete) 'backward-or-forward-kill-sexp)
(define-key global-map '(control meta kp-delete) 'backward-or-forward-kill-sexp)
;; or this one, either, on Linux.
(define-key global-map '(control meta backspace) 'backward-kill-sexp)


;;; Miscellaneous key bindings
(define-key global-map 'insert		'overwrite-mode)
(define-key global-map 'kp-insert	'overwrite-mode)
(define-key global-map 'again		'repeat-complex-command)
(define-key global-map 'redo		'repeat-complex-command)

(define-key global-map 'kp-enter	[return]) ; do whatever RET does now
(define-key global-map 'kp-tab		[tab])

(define-key global-map 'undo		'undo)
(define-key global-map 'help		'help-for-help)

(define-key global-map 'kp-space	'self-insert-command)
(define-key global-map 'kp-equal	'self-insert-command)
(define-key global-map 'kp-multiply	'self-insert-command)
(define-key global-map 'kp-add		'self-insert-command)
(define-key global-map 'kp-separator	'self-insert-command)
(define-key global-map 'kp-subtract	'self-insert-command)
(define-key global-map 'kp-decimal	'self-insert-command)
(define-key global-map 'kp-divide	'self-insert-command)

(define-key global-map 'kp-0		'self-insert-command)
(define-key global-map 'kp-1		'self-insert-command)
(define-key global-map 'kp-2		'self-insert-command)
(define-key global-map 'kp-3		'self-insert-command)
(define-key global-map 'kp-4		'self-insert-command)
(define-key global-map 'kp-5		'self-insert-command)
(define-key global-map 'kp-6		'self-insert-command)
(define-key global-map 'kp-7		'self-insert-command)
(define-key global-map 'kp-8		'self-insert-command)
(define-key global-map 'kp-9		'self-insert-command)

(define-key global-map 'select		'function-key-error)
(define-key global-map 'print		'function-key-error)
(define-key global-map 'execute		'execute-extended-command)
(define-key global-map 'clearline	'function-key-error)
(define-key global-map 'insertline	'open-line)
(define-key global-map 'deleteline	'kill-line)
(define-key global-map 'insertchar	'function-key-error)
(define-key global-map 'deletechar	'delete-char)
|#

;;; keydefs.el ends here

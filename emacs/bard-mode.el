;;; bard-mode.el --- Major mode for editing Bard

;; Copyright (C) 2012 mikel evins


;; (require 'bard-mode)

;; (setq bard-program-name (expand-file-name "~/bin/bard"))

;; (setq auto-mode-alist
;;       (cons '("\\.bard$" . bard-mode) auto-mode-alist))

;; (defun bard ()
;;   (interactive)
;;   (require 'bard-mode)
;;   (bard-repl))



(require 'scheme)
(require 'cmuscheme)
(require 'comint)
(require 'font-lock)

(defgroup bard nil
  "A Bard major mode."
  :group 'languages)

(defcustom bard-program-name "bard"
  "The Bard program used for evaluating code. Must be in your
path."
  :type 'string
  :group 'bard)

(defvar bard-buffer nil)

(defun bard-repl ()
  "Launch a Bard REPL using `bard-program-name' as an inferior mode."
  (interactive)

  (unless (comint-check-proc "*Bard REPL*")
    (setq bard-buffer
     (apply 'make-comint "Bard REPL"
            bard-program-name nil nil)))

  (pop-to-buffer "*Bard REPL*"))

(defun bard-interactively-start-process (&optional _cmd)
  (save-window-excursion
    (bard-repl (read-string "Run Bard: " bard-program-name))))

(defun bard-get-process ()
  "Return the current Scheme process or nil if none is running."
  (get-buffer-process bard-buffer))

(defun bard-proc ()
  (unless (and bard-buffer
               (get-buffer bard-buffer)
               (comint-check-proc bard-buffer))
    (bard-interactively-start-process))
  (or (bard-get-process)
      (error "No current process.  See variable `bard-buffer'")))

(defun bard-send-region (start end)
  (comint-send-region (bard-proc) start end)
  (comint-send-string (bard-proc) "\n"))

(defun bard-load-file (file-name)
  "Load a Bard file FILE-NAME into the inferior Bard process."
  (interactive (comint-get-source "Load Bard file: " nil '(bard-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs to be saved.
  (comint-send-string (bard-proc) (concat "(load \"" file-name "\"\)\n")))

;;; ---------------------------------------------------------------------
;;;  indentation
;;; ---------------------------------------------------------------------

(defun bard-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (if (and (eq (char-after (point)) ?\[)
                   (eq (char-after (elt state 1)) ?\())
              (+ (current-column) 2) ;; this is probably inside a defn
            (current-column)))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            (open-paren (elt state 1))
            method)
        (setq method (get (intern-soft function) 'bard-indent-function))

        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`\\(?:\\S +/\\)?def\\|with-"
                                      function)))
               (lisp-indent-defform state indent-point))

              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state)))))))

(put 'let 'bard-indent-function 1)

;;; ---------------------------------------------------------------------
;;; font-lock setup
;;; ---------------------------------------------------------------------

(setq bard-constants '("false" "nothing" "true" "undefined"))
(setq bard-constants-regexp (regexp-opt bard-constants 'words))

(setq bard-definitions '("define" "method" "macro" "schema" "variable"))
(setq bard-definitions-regexp (regexp-opt bard-definitions 'words))

(setq bard-control-forms '("and" "apply" "begin" "cond" "ensure" "eval" "if" "let" 
                           "load" "loop" "match" "not" "or" "receive" "repeat" "send"
                           "spawn" "unless" "values" "when" "with-exit" "with-open-file"))
(setq bard-control-forms-regexp (regexp-opt bard-control-forms 'words))

(setq bard-font-lock-keywords
      `(("*\\([[:word:]|[:punct:]]+\\)*" . font-lock-function-name-face)
        ("$\\([[:word:]|[:punct:]]+\\)" . font-lock-function-name-face)
        ("\\<[[:word:]|[:punct:]]+:" . font-lock-constant-face)
        ("<[[:word:]|[:punct:]]+>" . font-lock-type-face)
        (,bard-constants-regexp . font-lock-constant-face)
        (,bard-definitions-regexp . font-lock-keyword-face)
        (,bard-control-forms-regexp . font-lock-keyword-face)))

;;; ---------------------------------------------------------------------
;;; mode definition
;;; ---------------------------------------------------------------------

(define-derived-mode bard-mode scheme-mode
  "Bard"
  "Major mode for editing Bard."

  ;; key bindings
  (define-key bard-mode-map "\C-c\C-l" 'bard-load-file)
  (setq font-lock-defaults '(bard-font-lock-keywords))
  (setq mode-name "bard")
  (set (make-local-variable 'lisp-indent-function)
       'bard-indent-function))



(provide 'bard-mode)


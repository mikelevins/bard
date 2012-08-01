;;;----------------------------------------------------------------------------
;;; forked from gambit.el and modified
;;;----------------------------------------------------------------------------

;; These must be loaded first because we redefine some of the
;; functions they contain.

(require 'scheme)
(require 'cmuscheme)

;;;----------------------------------------------------------------------------

(defun bard-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a a symbol
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
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (bard-indent-method function)
                         (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond
         ((or (eq method 'defun)
              (eq method 'define)
              (eq method 'define-function)
              (eq method 'define-schema))
          (lisp-indent-defform state indent-point))
         
         ((integerp method)
          (lisp-indent-specform method state indent-point normal-indent))
         (method (funcall method state indent-point normal-indent)))))))

(defun bard-indent-method (function)
  (let ((method nil)
        (alist bard-indent-regexp-alist))
    (while (and (not method) (not (null alist)))
      (let* ((regexp (car alist))
             (x (string-match (car regexp) function)))
        (if x
            (setq method (cdr regexp)))
        (setq alist (cdr alist))))
    method))

(set lisp-indent-function 'bard-indent-function)

(defvar bard-indent-regexp-alist
  '(
    ("^declare$"               . defun)
    ("^##declare$"             . defun)
    ("^##define"               . defun)
    ("^macro-check"            . defun)
    ("^macro-force-vars$"      . defun)
    ("^macro-number-dispatch$" . defun)
   ))

;;;----------------------------------------------------------------------------

;; Redefine the function scheme-send-region from `cmuscheme' so
;; that we can keep track of all text sent to Bard's stdin.

(defun scheme-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (scheme-send-string (buffer-substring start end)))

(defun scheme-send-string (str)
  "Send a string to the inferior Scheme process."
  (let* ((clean-str (bard-string-terminate-with-newline str))
         (proc (scheme-proc))
         (pmark (process-mark proc))
         (buffer (get-buffer scheme-buffer))
         (old-buffer (current-buffer)))
    (set-buffer buffer)
    (goto-char pmark)
    (set-marker comint-last-input-start (point))
    (insert clean-str)
    (set-marker pmark (point))
    (bard-input-sender proc clean-str)
    (set-buffer old-buffer)))

(defun bard-input-sender (proc str)
  (let ((clean-str (bard-string-terminate-with-newline str)))
    (bard-register-input clean-str)
    (bard-make-read-only (current-buffer) (point-max))
    (bard-unhighlight)
    (comint-send-string proc clean-str)))

(defun bard-register-input (str)
  (let ((marker (make-marker)))
    (set-marker marker comint-last-input-start)
    (setq bard-input-line-marker-alist
          (cons (cons bard-input-line-count
                      marker)
                bard-input-line-marker-alist))
    (setq bard-input-line-count
          (+ bard-input-line-count
             (bard-string-count-lines str)))))

(defun bard-make-read-only (buffer end)
  ' ; disable read-only interaction, cause it doesn't work!
  (progn
    (put-text-property 1 end 'front-sticky '(read-only) buffer)
    (put-text-property 1 end 'rear-nonsticky '(read-only) buffer)
    (put-text-property 1 end 'read-only t buffer)))

;;;----------------------------------------------------------------------------

(defun bard-load-file (file-name)
  "Load a Bard file FILE-NAME into the inferior Bard process."
  (interactive (comint-get-source "Load Bard file: " scheme-prev-l/c-dir/file
                                  scheme-source-modes t)) ; T because LOAD
                                        ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (scheme-send-string (concat "(load \"" file-name "\"\)\n")))

;;;----------------------------------------------------------------------------

(defun bard-extend-mode-map (map)
  (define-key map "\C-c\C-l" 'bard-load-file))

(defun bard-inferior-mode ()
  (bard-extend-mode-map inferior-scheme-mode-map)
  (setq comint-input-sender (function bard-input-sender)))

(defun bard-mode ()
  (bard-install-comment-syntax)
  (bard-extend-mode-map scheme-mode-map))

;;(autoload 'bard-inferior-mode "bard" "Hook Bard mode into cmuscheme.")
;;(autoload 'bard-mode "bard" "Hook Bard mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function bard-inferior-mode))
(add-hook 'scheme-mode-hook (function bard-mode))

(provide 'bard)

;;;----------------------------------------------------------------------------

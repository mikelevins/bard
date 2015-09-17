(defun point-marker (&optional share (buffer (current-buffer)))
  (invoke buffer 'getPointMarker share))

(defun bolp (&optional (buffer (current-buffer)))
  (invoke (point-marker t buffer) 'isBeginningOfLine))

(defun eolp (&optional buffer)
  (invoke (point-marker t buffer) 'isEndOfLine))

(defun forward-char (&optional (count 1) (buffer (current-buffer)))
  (invoke (decode-buffer buffer) 'forwardChar count))

(defun backward-char (&optional (count 1) buffer)
  (invoke (decode-buffer buffer) 'backwardChar count))

(defun pos-visible-in-window-p (&optional pos window partially)
  t) ;; FIXME

(defun interactive-p () nil) ;;; FIXME

#|
(defun region-beginning (&optional buffer)
  (region-limit 1 (decode-buffer buffer)))

(defun region-end (&optional buffer)
  (region-limit 0 (decode-buffer buffer)))
|#

;;; FIXME
(defun mark-marker (&optional inactive-p (buffer (current-buffer)))
  (invoke buffer 'getMarkMarker inactive-p))

(defvar executing-kbd-macro nil)
(defvar zmacs-regions t)

(defvar last-command nil) ;;; FIXME

(defvar selective-display nil)

(defvar current-prefix-arg nil)

(defun barf-if-buffer-read-only (&optional buffer start end)
  nil) ;; FIXME

(defun execute-extended-command (prefix-arg)
  (interactive "P")
  (call-interactively (string->symbol (read-dialog "Command name:"))))

(defun lisp-interaction ()
  (interactive)
  (let ((buffer (get-buffer-create "*lisp-interaction*")))
    (invoke-static 'gnu.jemacs.buffer.ReplMode 'make buffer 'elisp)
    (use-local-map repl-map buffer)
    (switch-to-buffer buffer)
    buffer))

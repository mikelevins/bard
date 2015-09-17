;;; rect.el --- rectangle functions for XEmacs.

;; Copyright (C) 1985-2000 Free Software Foundation, Inc.

;; Maintainer: Didier Verna <didier@xemacs.org>
;; Keywords: internal

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

;;; Synched up with: to be incorporated in a forthcoming GNU Emacs

;;; Commentary:

;; This package provides the operations on rectangles that are documented
;; in the XEmacs Reference Manual.

;; #### NOTE: this file has been almost completely rewritten by Didier Verna
;; <didier@xemacs.org>, Jul 99. The purpose of this rewrite is to be less
;; intrusive and fill lines with whitespaces only when needed. A few functions
;; are untouched though, as noted above their definition.


;;; Code:

;; #### NOTE: this function is untouched, but not used anymore.
;; `apply-on-rectangle' is used instead. It's still there because it's
;; documented so people might use it in their code, so I've decided not to
;; touch it. --dv
;; XEmacs: extra-args
(defun operate-on-rectangle (function start end coerce-tabs &rest extra-args)
  "Call FUNCTION for each line of rectangle with corners at START, END.
If COERCE-TABS is non-nil, convert multi-column characters
that span the starting or ending columns on any line
to multiple spaces before calling FUNCTION.
FUNCTION is called with three arguments:
 position of start of segment of this line within the rectangle,
 number of columns that belong to rectangle but are before that position,
 number of columns that belong to rectangle but are after point.
Point is at the end of the segment of this line within the rectangle."
  (let (startcol startlinepos endcol endlinepos)
    (save-excursion
      (goto-char start)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startlinepos (point)))
    (save-excursion
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endlinepos (point-marker)))
    (if (< endcol startcol)
	;; XEmacs
	(let ((tem startcol))
	  (setq startcol endcol endcol tem)))
    (save-excursion
      (goto-char startlinepos)
      (while (< (point) endlinepos)
	(let (startpos begextra endextra)
	  (move-to-column startcol coerce-tabs)
	  (setq begextra (- (current-column) startcol))
	  (setq startpos (point))
	  (move-to-column endcol coerce-tabs)
	  (setq endextra (- endcol (current-column)))
	  (if (< begextra 0)
	      (setq endextra (+ endextra begextra)
		    begextra 0))
	  (if (< endextra 0) (setq endextra 0))
	  (apply function startpos begextra endextra extra-args))
	(forward-line 1)))
    (- endcol startcol)))

;; The replacement for `operate-on-rectangle' -- dv
(defun apply-on-rectangle (function start end &rest args)
  "Call FUNCTION for each line of rectangle with corners at START and END.
FUNCTION is called with two arguments: the start and end columns of the
rectangle, plus ARGS extra arguments. Point is at the beginning of line
when the function is called."
  (let (startcol startpt endcol endpt)
    (save-excursion
      (goto-char start)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startpt (point))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; ensure the start column is the left one.
      (if (< endcol startcol)
	  (let ((col startcol))
	    (setq startcol endcol endcol col)))
      ;; start looping over lines
      (goto-char startpt)
      (while (< (point) endpt)
	(apply function startcol endcol args)
	(forward-line 1)))
    ))


(defun delete-rectangle-line (startcol endcol fill)
  (let ((pt (point-at-eol)))
    (when (= (move-to-column startcol (or fill 'coerce)) startcol)
      (if (and (not fill) (<= pt endcol))
	  (delete-region (point) pt)
	;; else
	(setq pt (point))
	(move-to-column endcol t)
	(delete-region pt (point))))
    ))

;;;###autoload
(defun delete-rectangle (start end &optional fill)
  "Delete the text in the region-rectangle without saving it.
The same range of columns is deleted in each line starting with the line
where the region begins and ending with the line where the region ends.

When called from a program, the rectangle's corners are START and END.
With a prefix (or FILL) argument, also fill lines where nothing has to be
deleted."
  (interactive "*r\nP")
  (apply-on-rectangle 'delete-rectangle-line start end fill))


;; I love ascii art ;-)
(defconst spaces-strings '[""
			   " "
			   "  "
			   "   "
			   "    "
			   "     "
			   "      "
			   "       "
			   "        "])

;; This function is untouched --dv
(defun spaces-string (n)
  (if (<= n 8) (aref spaces-strings n)
    (let ((val ""))
      (while (> n 8)
	(setq val (concat "        " val)
	      n (- n 8)))
      (concat val (aref spaces-strings n)))))


(defun delete-extract-rectangle-line (startcol endcol lines fill)
  (let ((pt (point-at-eol)))
    (if (< (move-to-column startcol (or fill 'coerce)) startcol)
	(setcdr lines (cons (spaces-string (- endcol startcol))
			    (cdr lines)))
      ;; else
      (setq pt (point))
      (move-to-column endcol t)
      (setcdr lines (cons (buffer-substring pt (point)) (cdr lines)))
      (delete-region pt (point)))
    ))

;;;###autoload
(defun delete-extract-rectangle (start end &optional fill)
  "Delete the contents of the rectangle with corners at START and END, and
return it as a list of strings, one for each line of the rectangle.

With an optional FILL argument, also fill lines where nothing has to be
deleted."
  (let ((lines (list nil)))
    (apply-on-rectangle 'delete-extract-rectangle-line start end lines fill)
    (nreverse (cdr lines))))


;; #### NOTE: this is actually the only function that needs to do complicated
;; stuff like what's happening in `operate-on-rectangle', because the buffer
;; might be read-only. --dv
(defun extract-rectangle-line (startcol endcol lines)
  (let (start end begextra endextra line)
    (move-to-column startcol)
    (setq start (point)
	  begextra (- (current-column) startcol))
    (move-to-column endcol)
    (setq end (point)
	  endextra (- endcol (current-column)))
    (setq line (buffer-substring start (point)))
    (if (< begextra 0)
	(setq endextra (+ endextra begextra)
	      begextra 0))
    (if (< endextra 0)
	(setq endextra 0))
    (goto-char start)
    (while (search-forward "\t" end t)
      (let ((width (- (current-column)
		      (save-excursion (forward-char -1)
				      (current-column)))))
	(setq line (concat (substring line 0 (- (point) end 1))
			   (spaces-string width)
			   (substring line (+ (length line)
					      (- (point) end)))))))
    (if (or (> begextra 0) (> endextra 0))
	(setq line (concat (spaces-string begextra)
			   line
			   (spaces-string endextra))))
    (setcdr lines (cons line (cdr lines)))))

;;;###autoload
(defun extract-rectangle (start end)
  "Return the contents of the rectangle with corners at START and END,
as a list of strings, one for each line of the rectangle."
  (let ((lines (list nil)))
    (apply-on-rectangle 'extract-rectangle-line start end lines)
    (nreverse (cdr lines))))


;;;###autoload
(defvar killed-rectangle nil
  "Rectangle for `yank-rectangle' to insert.")

;;;###autoload
(defun kill-rectangle (start end &optional fill)
  "Delete the region-rectangle and save it as the last killed one.
You might prefer to use `delete-extract-rectangle' from a program.

When called from a program, the rectangle's corners are START and END.
With a prefix (or FILL) argument, also fill lines where nothing has to be
deleted."
  (interactive "*r\nP")
  (when buffer-read-only
    (setq killed-rectangle (extract-rectangle start end))
    (barf-if-buffer-read-only))
  (setq killed-rectangle (delete-extract-rectangle start end fill)))

;; This function is untouched --dv
;;;###autoload
(defun yank-rectangle ()
  "Yank the last killed rectangle with upper left corner at point."
  (interactive "*")
  (insert-rectangle killed-rectangle))


;; This function is untouched --dv
;;;###autoload
(defun insert-rectangle (rectangle)
  "Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point, its second
line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.
After this command, the mark is at the upper left corner
and point is at the lower right corner."
  (let ((lines rectangle)
	(insertcolumn (current-column))
	(first t))
    (push-mark)
    (while lines
      (or first
	  (progn
	    (forward-line 1)
	    (or (bolp) (insert ?\n))
	    (move-to-column insertcolumn t)))
      (setq first nil)
      (insert (car lines))
      (setq lines (cdr lines)))))


(defun open-rectangle-line (startcol endcol fill)
  (let (spaces)
    (when (= (move-to-column startcol (or fill 'coerce)) startcol)
      (unless (and (not fill)
		   (= (point) (point-at-eol)))
	(indent-to endcol)))
    ))

;;;###autoload
(defun open-rectangle (start end &optional fill)
  "Blank out the region-rectangle, shifting text right.

When called from a program, the rectangle's corners are START and END.
With a prefix (or FILL) argument, fill with blanks even if there is no text
on the right side of the rectangle."
  (interactive "*r\nP")
  (apply-on-rectangle 'open-rectangle-line start end fill)
  (goto-char start))


(defun string-rectangle-line (startcol endcol string delete)
  (move-to-column startcol t)
  (if delete
      (delete-rectangle-line startcol endcol nil))
  (insert string))

;;;###autoload
(defun string-rectangle (start end string)
  "Insert STRING on each line of the region-rectangle, shifting text right.
The left edge of the rectangle specifies the column for insertion.

If `pending-delete-mode' is active the string replace the region.
Otherwise this command does not delete or overwrite any existing text.

When called from a program, the rectangle's corners are START and END."
  (interactive "*r\nsString rectangle: ")
  (apply-on-rectangle 'string-rectangle-line start end string
                      (and (boundp 'pending-delete-mode) pending-delete-mode)))

(defun replace-rectangle (start end string)
  "Like `string-rectangle', but unconditionally replace the original region,
as if `pending-delete-mode' were active."
  (interactive "*r\nsString rectangle: ")
  (apply-on-rectangle 'string-rectangle-line start end string t))


(defun clear-rectangle-line (startcol endcol fill)
  (let ((pt (point-at-eol))
	spaces)
    (when (= (move-to-column startcol (or fill 'coerce)) startcol)
      (if (and (not fill)
	       (<= (save-excursion (goto-char pt) (current-column)) endcol))
	  (delete-region (point) pt)
	;; else
	(setq pt (point))
	(move-to-column endcol t)
	(setq spaces (- (point) pt))
	(delete-region pt (point))
	(indent-to (+ (current-column) spaces))))
    ))

;;;###autoload
(defun clear-rectangle (start end &optional fill)
  "Blank out the region-rectangle.
The text previously in the region is overwritten with blanks.

When called from a program, the rectangle's corners are START and END.
With a prefix (or FILL) argument, also fill with blanks the parts of the
rectangle which were empty."
  (interactive "*r\nP")
  (apply-on-rectangle 'clear-rectangle-line start end fill))


(provide 'rect)

;;; rect.el ends here

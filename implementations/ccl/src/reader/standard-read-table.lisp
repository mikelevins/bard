;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          standard-read-table.lisp
;;;; Project:       Bard
;;;; Purpose:       set up the standard Bard read table
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun init-bard-read-table ()

;;; ---------------------------------------------------------------------
;;; comments
;;; ---------------------------------------------------------------------

(set-reader-for! #\; *bard-read-table* #'read-line-comment)

;;; (with-input-from-string (in "; foo") (read in))

;;; ---------------------------------------------------------------------
;;; numbers
;;; ---------------------------------------------------------------------

(set-reader-for! #\0 *bard-read-table* #'read-digit)
(set-reader-for! #\1 *bard-read-table* #'read-digit)
(set-reader-for! #\2 *bard-read-table* #'read-digit)
(set-reader-for! #\3 *bard-read-table* #'read-digit)
(set-reader-for! #\4 *bard-read-table* #'read-digit)
(set-reader-for! #\5 *bard-read-table* #'read-digit)
(set-reader-for! #\6 *bard-read-table* #'read-digit)
(set-reader-for! #\7 *bard-read-table* #'read-digit)
(set-reader-for! #\8 *bard-read-table* #'read-digit)
(set-reader-for! #\9 *bard-read-table* #'read-digit)

;;; (with-input-from-string (in "12.34") (read in))
;;; (with-input-from-string (in "12(.34)") (read in))
;;; (with-input-from-string (in "12A.34)") (read in))

;;; ---------------------------------------------------------------------
;;; names
;;; ---------------------------------------------------------------------

(set-reader-for! #\! *bard-read-table* #'read-name)
(set-reader-for! #\@ *bard-read-table* #'read-name)
(set-reader-for! #\$ *bard-read-table* #'read-name)
(set-reader-for! #\% *bard-read-table* #'read-name)
(set-reader-for! #\^ *bard-read-table* #'read-name)
(set-reader-for! #\& *bard-read-table* #'read-name)
(set-reader-for! #\* *bard-read-table* #'read-name)
(set-reader-for! #\_ *bard-read-table* #'read-name)
(set-reader-for! #\+ *bard-read-table* #'read-name)
(set-reader-for! #\- *bard-read-table* #'read-name)
(set-reader-for! #\= *bard-read-table* #'read-name)
(set-reader-for! #\Q *bard-read-table* #'read-name)
(set-reader-for! #\q *bard-read-table* #'read-name)
(set-reader-for! #\W *bard-read-table* #'read-name)
(set-reader-for! #\w *bard-read-table* #'read-name)
(set-reader-for! #\E *bard-read-table* #'read-name)
(set-reader-for! #\e *bard-read-table* #'read-name)
(set-reader-for! #\R *bard-read-table* #'read-name)
(set-reader-for! #\r *bard-read-table* #'read-name)
(set-reader-for! #\T *bard-read-table* #'read-name)
(set-reader-for! #\t *bard-read-table* #'read-name)
(set-reader-for! #\Y *bard-read-table* #'read-name)
(set-reader-for! #\y *bard-read-table* #'read-name)
(set-reader-for! #\U *bard-read-table* #'read-name)
(set-reader-for! #\u *bard-read-table* #'read-name)
(set-reader-for! #\I *bard-read-table* #'read-name)
(set-reader-for! #\i *bard-read-table* #'read-name)
(set-reader-for! #\O *bard-read-table* #'read-name)
(set-reader-for! #\o *bard-read-table* #'read-name)
(set-reader-for! #\P *bard-read-table* #'read-name)
(set-reader-for! #\p *bard-read-table* #'read-name)
(set-reader-for! #\A *bard-read-table* #'read-name)
(set-reader-for! #\a *bard-read-table* #'read-name)
(set-reader-for! #\S *bard-read-table* #'read-name)
(set-reader-for! #\s *bard-read-table* #'read-name)
(set-reader-for! #\D *bard-read-table* #'read-name)
(set-reader-for! #\d *bard-read-table* #'read-name)
(set-reader-for! #\F *bard-read-table* #'read-name)
(set-reader-for! #\f *bard-read-table* #'read-name)
(set-reader-for! #\G *bard-read-table* #'read-name)
(set-reader-for! #\g *bard-read-table* #'read-name)
(set-reader-for! #\H *bard-read-table* #'read-name)
(set-reader-for! #\h *bard-read-table* #'read-name)
(set-reader-for! #\J *bard-read-table* #'read-name)
(set-reader-for! #\j *bard-read-table* #'read-name)
(set-reader-for! #\K *bard-read-table* #'read-name)
(set-reader-for! #\k *bard-read-table* #'read-name)
(set-reader-for! #\L *bard-read-table* #'read-name)
(set-reader-for! #\l *bard-read-table* #'read-name)
(set-reader-for! #\Z *bard-read-table* #'read-name)
(set-reader-for! #\z *bard-read-table* #'read-name)
(set-reader-for! #\X *bard-read-table* #'read-name)
(set-reader-for! #\x *bard-read-table* #'read-name)
(set-reader-for! #\C *bard-read-table* #'read-name)
(set-reader-for! #\c *bard-read-table* #'read-name)
(set-reader-for! #\V *bard-read-table* #'read-name)
(set-reader-for! #\v *bard-read-table* #'read-name)
(set-reader-for! #\B *bard-read-table* #'read-name)
(set-reader-for! #\b *bard-read-table* #'read-name)
(set-reader-for! #\N *bard-read-table* #'read-name)
(set-reader-for! #\n *bard-read-table* #'read-name)
(set-reader-for! #\M *bard-read-table* #'read-name)
(set-reader-for! #\m *bard-read-table* #'read-name)
(set-reader-for! #\< *bard-read-table* #'read-name)
(set-reader-for! #\> *bard-read-table* #'read-name)
(set-reader-for! #\? *bard-read-table* #'read-name)

;;; (with-input-from-string (in "nothing") (read in))
;;; (with-input-from-string (in "true") (read in))
;;; (with-input-from-string (in "false") (read in))
;;; (with-input-from-string (in "FooBar") (read in))
;;; (with-input-from-string (in "+") (read in))


)
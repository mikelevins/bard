;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-streams.lisp
;;;; Project:       Bard
;;;; Purpose:       I/O and stream primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(defun directory? (pathspec)
  (cl-fad:directory-pathname-p pathspec))

;;; ---------------------------------------------------------------------
;;; I/O ops
;;; ---------------------------------------------------------------------

(defun display (x) (princ x))
(defun bard-write (x) (princ (value->literal-string x)))
(defun newline () (terpri))

;;; ---------------------------------------------------------------------
;;; streams
;;; ---------------------------------------------------------------------

(defclass <stream> ()
  ((url :accessor stream-url :initform nil :initarg :url)
   (stream :accessor base-stream :initform nil :initarg :stream)
   (element-type :accessor element-type :initform 'bard-symbols::|character| :initarg :element-type)))

(defclass <file-stream> (<stream>)
  ((pathname :accessor stream-pathname :initform nil :initarg :pathname)))

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(defun stream.standard-input ()
  (make-instance '<stream> :url nil :stream cl:*standard-input*))

(defun stream.standard-output ()
  (make-instance '<stream> :url nil :stream cl:*standard-output*))

(defun stream.standard-error ()
  (make-instance '<stream> :url nil :stream cl:*error-output*))

(defmethod make-stream (scheme url direction element-type)
  (error "Unsupported stream specification: ~s"
         (list scheme url direction element-type)))

(defmethod make-stream ((scheme (eql 'bard-symbols::|file|)) 
                        (url puri:uri) 
                        (direction (eql 'bard-symbols::|input|))
                        (element-type (eql 'bard-symbols::|character|)))
  (let* ((truename (probe-file (puri:uri-path url))))
    (if truename
        (if (directory? truename)
            (error "URL names a directory: ~a" url)
            (make-instance '<file-stream> :url url :pathname truename :element-type element-type))
        (error "No such file ~a" url))))

(defmethod stream.create ((url puri:uri)(direction symbol)(element-type symbol))
  (make-stream (url.scheme url) url direction element-type))

(defmethod stream.open? ((s <stream>))
  (and (base-stream s)
       (open-stream-p (base-stream s))))

;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(defmethod %as-lisp-element-type ((etype (eql 'bard-symbols::|octet|)))
  'cl:unsigned-byte)

(defmethod %as-lisp-element-type ((etype (eql 'bard-symbols::|character|)))
  'cl:character)

(defmethod stream.element-type ((s <stream>)) 
  (element-type s))

(defmethod stream.length ((s <file-stream>)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s))))
        (elt-type (%as-lisp-element-type (element-type s))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type elt-type)
      (file-length in))))

(defmethod stream.read-octet ((s <file-stream>)(pos integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type '(unsigned-byte 8))
      (let ((success? (file-position in pos)))
        (if success?
            (read-byte in)
            (error "Invalid file position: ~s" pos))))))

;;; NOTE: CCL-specific code
(defmethod stream.read-octets ((s <file-stream>)(pos integer)(count integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type '(unsigned-byte 8))
      (let ((len (file-length in))
            (success? (file-position in pos)))
        (if success?
            (if (<= count (- len pos))
                (let* ((buf (make-array count :element-type '(unsigned-byte 8) :initial-element 0))
                       (octets-read (ccl:stream-read-ivector in buf 0 count)))
                  (if (> octets-read 0)
                      (subseq buf 0 octets-read)
                      (error "Unable to read octets from stream: ~s" s)))
                (error "Unable to read ~a octets from stream: ~s" count s))
            (error "Invalid file position: ~s" pos))))))

;;; NOTE: CCL-specific code
(defmethod stream.read-all-octets ((s <file-stream>)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type '(unsigned-byte 8))
      (let ((len (file-length in)))
        (let* ((buf (make-array len :element-type '(unsigned-byte 8) :initial-element 0))
               (octets-read (ccl:stream-read-ivector in buf 0 len)))
          (if (> octets-read 0)
              (values (subseq buf 0 octets-read) octets-read)
              (error "Unable to read octets from stream: ~s" s)))))))

(defmethod stream.read-character ((s <file-stream>)(pos integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (let ((success? (file-position in pos)))
        (if success?
            (read-char in)
            (error "Invalid file position: ~s" pos))))))

(defmethod stream.read-characters ((s <file-stream>)(pos integer)(count integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (let ((len (file-length in))
            (success? (file-position in pos)))
        (if success?
            (if (<= count (- len pos))
                (let* ((buf (make-array count :element-type 'cl:character :initial-element #\null))
                       (chars-read (read-sequence buf in :start 0 :end count)))
                  (if (> chars-read 0)
                      (subseq buf 0 chars-read)
                      (error "Unable to read characters from stream: ~s" s)))
                (error "Unable to read ~a characters from stream: ~s" count s))
            (error "Invalid file position: ~s" pos))))))

(defmethod stream.read-all-characters ((in <file-stream>)) )

(defmethod stream.read-line ((s <file-stream>)(pos integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (let ((success? (file-position in pos)))
        (if success?
            (read-line in)
            (error "Invalid file position: ~s" pos))))))

(defmethod stream.read-lines ((s <file-stream>)(pos integer)(count integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s))))
        (eof (gensym)))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      ;; skip over the objects before pos 
      (let ((line-count 0))
        (block counting
          (loop (if (< line-count pos)
                    (let ((line (read-line in nil eof)))
                      (if (eql line eof)
                          (return-from counting nil)
                          (incf line-count)))
                    (return-from counting nil)))))
      ;; collect count lines
      (let ((line-count 0)
            (lines nil))
        (block collecting
          (loop (if (< line-count count)
                    (let ((line (read-line in nil eof)))
                      (if (eql line eof)
                          (return-from collecting (reverse lines))
                          (progn
                            (push line lines)
                            (incf line-count))))
                    (return-from collecting (reverse lines)))))))))

(defmethod stream.read-all-lines ((s <file-stream>)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (let ((eof (gensym)))
        (loop for line = (read-line in nil eof)
           until (eq line eof)
           collect line)))))

(defmethod stream.read-object ((s <file-stream>)(pos integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (let ((success? (file-position in pos)))
        (if success?
            (bard-read in)
            (error "Invalid file position: ~s" pos))))))

(defmethod stream.read-objects ((s <file-stream>)(pos integer)(count integer)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      ;; skip over the objects before pos 
      (let ((obj-count 0))
        (block counting
          (loop (if (< obj-count pos)
                    (let ((obj (bard-read in)))
                      (if (typep obj (find-class '<eof>))
                          (return-from counting nil)
                          (incf obj-count)))
                    (return-from counting nil)))))
      ;; collect count objects
      (let ((obj-count 0)
            (objs nil))
        (block collecting
          (loop (if (< obj-count count)
                    (let ((obj (bard-read in)))
                      (if (typep obj (find-class '<eof>))
                          (return-from collecting (reverse objs))
                          (progn
                            (push obj objs)
                            (incf obj-count))))
                    (return-from collecting (reverse objs)))))))))

(defmethod stream.read-all-objects ((s <file-stream>)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (loop for obj = (bard-read in)
         until (typep obj (find-class '<eof>))
         collect obj))))

(defmethod stream.write-octet ((in <file-stream>)(octet integer)) )
(defmethod stream.write-octets ((in <file-stream>) (octets sequence)) )

(defmethod stream.write-character ((in <file-stream>)(ch character)) )
(defmethod stream.write-characters ((in <file-stream>) (characters sequence)) )

(defmethod stream.write-line ((in <file-stream>)(line string)) )
(defmethod stream.write-lines ((in <file-stream>)(lines sequence)) )

(defmethod stream.write-object ((in <file-stream>) object) )
(defmethod stream.write-objects ((in <file-stream>)(objects sequence)) )

;;; ---------------------------------------------------------------------
;;; converters
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|display| 1
    (make-prim :name 'bard-symbols::|display|
               :n-args 1
               :opcode 'bard::display
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|newline| 0
    (make-prim :name 'bard-symbols::|newline|
               :n-args 0
               :opcode 'bard::newline
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|read| 0
    (make-prim :name 'bard-symbols::|read|
               :n-args 0
               :opcode 'bard::bard-read
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|write| 1
    (make-prim :name 'bard-symbols::|write|
               :n-args 1
               :opcode 'bard::bard-write
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|stream.standard-input| 0
    (make-prim :name 'bard-symbols::|stream.standard-input|
               :n-args 0
               :opcode 'bard::stream.standard-input
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.standard-output| 0
    (make-prim :name 'bard-symbols::|stream.standard-output|
               :n-args 0
               :opcode 'bard::stream.standard-output
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.standard-error| 0
    (make-prim :name 'bard-symbols::|stream.standard-error|
               :n-args 0
               :opcode 'bard::stream.standard-error
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|make-stream| 4
    (make-prim :name 'bard-symbols::|make-stream|
               :n-args 4
               :opcode 'bard::make-stream
               :always t
               :side-effects nil))

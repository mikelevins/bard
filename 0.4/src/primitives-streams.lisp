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

(defmethod make-stream (scheme url element-type)
  (error "Unsupported stream specification: ~s"
         (list scheme url element-type)))

(defmethod make-stream ((scheme (eql 'bard-symbols::|file|)) 
                        (url puri:uri) 
                        (element-type (eql 'bard-symbols::|character|)))
  (let* ((path (puri:uri-path url)))
    (if (directory? path)
        (error "URL names a directory: ~a" url)
        (make-instance '<file-stream> :url url :pathname path :element-type element-type))))

(defmethod stream.create ((url puri:uri)(element-type (eql 'bard-symbols::|character|)))
  (make-stream (url.scheme url) url element-type))

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

(defmethod stream.read-all-characters ((s <file-stream>)) 
  (let ((truename (probe-file (puri:uri-path (stream-url s)))))
    (assert truename () "No such file: ~a" (stream-url s))
    (with-open-file (in truename :direction :input :element-type 'cl:character)
      (let ((len (file-length in)))
        (let* ((buf (make-array len :element-type 'cl:character :initial-element 0))
               (chars-read (read-sequence buf in :start 0 :end len)))
          (if (> chars-read 0)
              (values (subseq buf 0 chars-read) chars-read)
              (error "Unable to read characters from stream: ~s" s)))))))

(defmethod stream.read-line ((s <file-stream>)(pos integer)) 
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
      ;; read the next line
      (read-line in nil eof))))

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
      ;; skip over the objects before pos 
      (let ((obj-count 0))
        (block counting
          (loop (if (< obj-count pos)
                    (let ((obj (bard-read in)))
                      (if (typep obj (find-class '<eof>))
                          (return-from counting nil)
                          (incf obj-count)))
                    (return-from counting nil)))))
      ;; read the next object
      (bard-read in))))

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

(defmethod stream.write-octet ((s <file-stream>)(pos integer)(octet integer)) 
  (assert (typep octet 'octet)() "Invalid octet argument to stream.write-octet: ~s" octet)
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :overwrite :if-does-not-exist :create)
      (let ((success? (file-position out pos)))
        (if success?
            (write-byte octet out)
            (error "Can't write an octet on stream ~S at position ~S"
                   s pos))))))

(defmethod stream.write-octets ((s <file-stream>)(pos integer)(octets sequence)) 
  (assert (every #'(lambda (o)(typep o 'octet)) octets)() "Invalid octet arguments in stream.write-octets: ~s" octets)
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :overwrite :if-does-not-exist :create)
      (let ((success? (file-position out pos)))
        (if success?
            (write-sequence octets out)
            (error "Can't write octets on stream ~S at position ~S"
                   s pos))))))

(defmethod stream.write-character ((s <file-stream>)(pos integer)(ch character)) 
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :output :element-type 'cl:character
                         :if-exists :overwrite :if-does-not-exist :create)
      (let ((success? (file-position out pos)))
        (if success?
            (write-char ch out)
            (error "Can't write a character on stream ~S at position ~S"
                   s pos))))))

(defmethod stream.write-characters ((s <file-stream>)(pos integer)(characters string)) 
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :output :element-type 'cl:character
                         :if-exists :overwrite :if-does-not-exist :create)
      (let ((success? (file-position out pos)))
        (if success?
            (write-sequence characters out)
            (error "Can't write characters on stream ~S at position ~S"
                   s pos))))))

(defmethod %stream.go-to-line ((s stream)(pos integer))
  (let ((line-count 0))
    (block searching
      (loop (if (>= line-count pos)
                (return-from searching
                  (file-position s))
                (progn (read-line s)
                       (incf line-count)))))))

(defmethod stream.write-line ((s <file-stream>)(pos integer)(line string)) 
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :io :element-type 'cl:character
                         :if-exists :overwrite :if-does-not-exist :create)
      (let ((success? (%stream.go-to-line out pos)))
        (if success?
            (write-line line out)
            (error "Can't write a line on stream ~S at position ~S"
                   s pos))))))

(defmethod stream.write-lines ((s <file-stream>)(pos integer)(lines sequence)) 
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :io :element-type 'cl:character
                         :if-exists :overwrite :if-does-not-exist :create)
      (let ((success? (%stream.go-to-line out pos)))
        (if success?
            (dotimes (i (length lines))
              (write-line (elt lines i) out))
            (error "Can't write lines on stream ~S at position ~S"
                   s pos))))))

(defmethod %stream.go-to-object ((s stream)(pos integer))
  (let ((object-count 0))
    (block searching
      (loop (if (>= object-count pos)
                (return-from searching
                  (file-position s))
                (progn (bard-read s)
                       (incf object-count)))))))

(defmethod stream.write-object ((s <file-stream>)(pos integer) object) 
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :io :element-type 'cl:character
                         :if-exists :append :if-does-not-exist :create)
      (let ((success? (%stream.go-to-object out pos)))
        (if success?
            (let ((str (value->literal-string object)))
              (write-string str out))
            (error "Can't write object on stream ~S at position ~S"
                   s pos))))))

(defmethod stream.write-objects ((s <file-stream>)(pos integer)(objects sequence)) 
  (let ((path (puri:uri-path (stream-url s))))
    (with-open-file (out path :direction :io :element-type 'cl:character
                         :if-exists :append :if-does-not-exist :create)
      (let ((success? (%stream.go-to-object out pos)))
        (if success?
            (dotimes (i (length objects))
              (let ((str (value->literal-string (elt objects i))))
                (write-string str out)))
            (error "Can't write object on stream ~S at position ~S"
                   s pos))))))

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

(defprim 'bard-symbols::|stream.create| 2
    (make-prim :name 'bard-symbols::|stream.create|
               :n-args 2
               :opcode 'bard::stream.create
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.length| 1
    (make-prim :name 'bard-symbols::|stream.length|
               :n-args 1
               :opcode 'bard::stream.length
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-octet| 2
    (make-prim :name 'bard-symbols::|stream.read-octet|
               :n-args 2
               :opcode 'bard::stream.read-octet
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-octets| 3
    (make-prim :name 'bard-symbols::|stream.read-octets|
               :n-args 3
               :opcode 'bard::stream.read-octets
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-all-octets| 1
    (make-prim :name 'bard-symbols::|stream.read-all-octets|
               :n-args 1
               :opcode 'bard::stream.read-all-octets
               :always t
               :side-effects nil))


(defprim 'bard-symbols::|stream.read-character| 2
    (make-prim :name 'bard-symbols::|stream.read-character|
               :n-args 2
               :opcode 'bard::stream.read-character
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-characters| 3
    (make-prim :name 'bard-symbols::|stream.read-characters|
               :n-args 3
               :opcode 'bard::stream.read-characters
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-all-characters| 1
    (make-prim :name 'bard-symbols::|stream.read-all-characters|
               :n-args 1
               :opcode 'bard::stream.read-all-characters
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-line| 2
    (make-prim :name 'bard-symbols::|stream.read-line|
               :n-args 2
               :opcode 'bard::stream.read-line
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-lines| 3
    (make-prim :name 'bard-symbols::|stream.read-lines|
               :n-args 3
               :opcode 'bard::stream.read-lines
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-all-lines| 1
    (make-prim :name 'bard-symbols::|stream.read-all-lines|
               :n-args 1
               :opcode 'bard::stream.read-all-lines
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-object| 2
    (make-prim :name 'bard-symbols::|stream.read-object|
               :n-args 2
               :opcode 'bard::stream.read-object
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-objects| 3
    (make-prim :name 'bard-symbols::|stream.read-objects|
               :n-args 3
               :opcode 'bard::stream.read-objects
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.read-all-objects| 1
    (make-prim :name 'bard-symbols::|stream.read-all-objects|
               :n-args 1
               :opcode 'bard::stream.read-all-objects
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-octet| 3
    (make-prim :name 'bard-symbols::|stream.write-octet|
               :n-args 3
               :opcode 'bard::stream.write-octet
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-octets| 3
    (make-prim :name 'bard-symbols::|stream.write-octets|
               :n-args 3
               :opcode 'bard::stream.write-octets
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-character| 3
    (make-prim :name 'bard-symbols::|stream.write-character|
               :n-args 3
               :opcode 'bard::stream.write-character
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-characters| 3
    (make-prim :name 'bard-symbols::|stream.write-characters|
               :n-args 3
               :opcode 'bard::stream.write-characters
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-line| 3
    (make-prim :name 'bard-symbols::|stream.write-line|
               :n-args 3
               :opcode 'bard::stream.write-line
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-lines| 3
    (make-prim :name 'bard-symbols::|stream.write-lines|
               :n-args 3
               :opcode 'bard::stream.write-lines
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-object| 3
    (make-prim :name 'bard-symbols::|stream.write-object|
               :n-args 3
               :opcode 'bard::stream.write-object
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|stream.write-objects| 3
    (make-prim :name 'bard-symbols::|stream.write-objects|
               :n-args 3
               :opcode 'bard::stream.write-objects
               :always t
               :side-effects nil))

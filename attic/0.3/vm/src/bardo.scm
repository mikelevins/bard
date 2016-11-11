;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardo.scm
;;;; Project:       Bard
;;;; Purpose:       saving and loading object code
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type bardo
  id: 561393BE-A970-4D65-AD20-F59E3D6D7D3A
  version
  value)

(define (value->bardo val)
  (let ((bardo (make-bardo $bardo-version val)))
    (object->u8vector bardo)))

(define (bardo->value bardo)
  (bardo-value (u8vector->object bardo)))

(define (read-bardo path)
  (let* ((in #f)
         (data-bytes #f)
         (chunks (list))
         (buffer-length 4096)
         (data-buffer (make-u8vector buffer-length 0)))
    (dynamic-wind
        (lambda () (set! in (open-input-file path)))
        (lambda ()
          ;; read bytes from the file
          (do ((byte-count (read-subu8vector data-buffer 0 buffer-length in)
                           (read-subu8vector data-buffer 0 buffer-length in)))
              ((< byte-count buffer-length)
               ;; append the last chunk
               (set! chunks (cons (subu8vector data-buffer 0 byte-count)
                                  chunks)))
            (set! chunks (cons (subu8vector data-buffer 0 byte-count)
                               chunks))))
        (lambda () (close-input-port in)))
    ;; convert the bytes to a scheme object and return it
    (if (not (null? chunks))
        (apply u8vector-append (reverse chunks))
        #f)))

(define (load-bardo path)
  (bardo->value (read-bardo path)))

(define (write-bardo-file bardo dest-path)
  (let* ((byte-count (u8vector-length bardo))
         (out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () (write-subu8vector bardo 0 byte-count out))
        (lambda () (close-output-port out)))
    dest-path))


(define (save-bardo val path)
  (write-bardo-file (value->bardo val)
                    path))

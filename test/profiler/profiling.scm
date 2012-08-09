;; statprof.scm -- A statistical profiler for Gambit-C 4.0
;; See the README file for license and usage information.
;;
;; Adapted to Gambit 4.6 and Blackhole module system by Álvaro Castro-Castilla
;;
;; ----------------------------------------------------------------------------
;; Profiling & interruption handling

;;;(import (std string/sxml-to-xml))

(define *buckets* #f)
(define *total* 0)

(##thread-heartbeat-interval-set! (exact->inexact 1/1000))

(define (profile-start!)
  (set! *buckets* '())
  (set! *total* 0)
  (##interrupt-vector-set! 1 profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

;; (define (identify-continuation cont) ; first version
;;   (or (##continuation-creator cont)
;;       'unknown))

(define (identify-continuation cont) ; second version
  (let ((locat (##continuation-locat cont)))
    (if locat
        (let* ((container (##locat-container locat))
               (file (##container->path container)))
          (if file
              (let* ((filepos (##position->filepos (##locat-position locat)))
                     (line (##fixnum.+ (##filepos-line filepos) 1))
                     (col (##fixnum.+ (##filepos-col filepos) 1)))
                (list file line col))
              'unknown))
        'unknown)))

(define (profile-heartbeat!)
  (##continuation-capture
   (lambda (cont)
     (##thread-heartbeat!)
     (let ((id (identify-continuation cont)))
       (if (not (eq? id 'unknown))
           (let ((bucket (assoc (car id) *buckets*)))
             (set! *total* (+ *total* 1))
             (if (not bucket)
                 (begin
                   (set! *buckets* (cons 
                                    (cons (car id) 
                                          ;; fixme: arbitrary hard limit
                                          ;; on the length of source
                                          ;; files
                                          (make-vector 5000 0)) 
                                    *buckets*))
                   (set! bucket (car *buckets*))))

             (vector-set! (cdr bucket)
                          (cadr id) 
                          (+ (vector-ref (cdr bucket) 
                                         (cadr id))
                             1))))))))


;; ----------------------------------------------------------------------------
;; Text formatting

(define (pad-left s l c)
  (let loop ((s (string->list s)))
    (if (< (length s) l)
        (loop (cons c s))
        (list->string s))))


;; ----------------------------------------------------------------------------
;; Palette generation & color formatting

(define (gradient from to step)
  (let ((inc (map (lambda (x) (/ x step))
                  (map - to from))))
    
    (let loop ((i 0)
               (acc '()))
      (if (= i step) 
          (reverse acc)
          (loop (+ i 1)
                (cons (map 
                       (lambda (x o) 
                         (round (+ x (* i o))))
                       from
                       inc)
                      acc))))))

(define (as-rgb col)
  (apply string-append
         (map
          (lambda (x)
            (pad-left (number->string x 16) 2 #\0))
          col)))

(define palette
  (list->vector 
   (cons '(255 255 255) 
         (gradient '(127 127 255) 
                   '(255 127 127)
                   16))))


;; ----------------------------------------------------------------------------
;; Functions to generate the report

(define (write-profile-report profile-name)

  (define (iota1 n)
    (let loop ((n n)
               (l '()))
      (if (>= n 1) 
          (loop (- n 1) (cons n l))
          l)))
  
  (define directory-name (string-append (current-directory)
                                        profile-name
                                        "/"))
  (with-exception-catcher
   (lambda (e)
     ;; ignore the exception, it probably means that the directory
     ;; already existed.  If there's another problem it will be
     ;; signaled later.
     #f) 
   (lambda ()
     (create-directory (list path: directory-name
                             permissions: #o755))))
  
  (let ((max-intensity 
         (apply max
                (cons 1.1 (map
                 (lambda (data)
                   (apply max 
                          (cons 1.1 (vector->list data))))
                 (map cdr *buckets*))))))

    (map 
     (lambda (bucket)
       (let ((file (car bucket))
             (data (cdr bucket)))
         
         (define (get-color n)
           (let ((i (vector-ref data n)))
             (if (= i 0)
                 (as-rgb (vector-ref palette 0))
                 (let ((x (* (/ (log (+ 1. i))
                                (ceiling (log max-intensity)))
                             (- (vector-length palette) 1))))
                   (as-rgb (vector-ref palette 
                                       (min
                                        (inexact->exact (floor x))
                                        (- (vector-length palette) 1))))))))

         (with-output-to-file (string-append 
                               directory-name
                               (path-strip-directory file)
                               ".html")
           (let ((lines (call-with-input-file file 
                          (lambda (p) (read-all p read-line)))))
             (lambda ()
               (display
                (sxml>>xml
                 `(html 
                   (body
                    (table 
                     (@ (cellspacing "0")
                        (cellpadding "0")
                        (border "0")
                        (style "font-size: 12px;"))
                     ,@(map
                        (lambda (line linea)
                          `(tr 
                            (td ,(string-append 
                                  (number->string linea)
                                  ": "))
                            ;; (td 
                            ;;  align: center
                            ;;  ,(let ((n (vector-ref data linea)))
                            ;;     (if (= n 0)
                            ;;         ""
                            ;;         (string-append "[" 
                            ;;                        (number->string n)
                            ;;                        "/"
                            ;;                        (number->string *total*)
                            ;;                        "]"))))
                            
                            (td 
                             (@ (align "center"))
                             ,(let ((n (vector-ref data linea)))
                                (if (= n 0)
                                    ""
                                    (string-append
                                     (number->string
                                      (round% (/ n *total*)))
                                     "% "))))
                            
                            (td (pre (@ (style ,(string-append     
                                                 "background-color:#"
                                                 (get-color linea))))
                                     ,line))))
                        lines
                        (iota1 (length lines)))))))))))))
     
     *buckets*))

  (with-output-to-file (string-append directory-name "index.html")
    (lambda ()
      (display
       (sxml>>xml
        `(html
          (body
           ,@(map (lambda (bucket)
                    (let ((file-path (string-append 
                                      directory-name
                                      (path-strip-directory (car bucket)) 
                                      ".html")))
                      `(p (a (@ (href ,file-path ,file-path)))
						  ,(path-strip-directory (car bucket))
                          " ["
                          ,(round%
                            (/ (apply + (vector->list (cdr bucket)))
                               *total*)) 
                          " %]")))
                  *buckets*))))))))

(define (round% n)
  (/ (round
      (* 10000 n))
     100.))


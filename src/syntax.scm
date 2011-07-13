;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.scm
;;;; Project:       bard
;;;; Purpose:       representation of Bard syntax objects
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (<syntax-atom>:make #!key syntax-type value)
  (make-frame type: '<syntax-atom> syntax-type: syntax-type value: value))

(define (<syntax-sequence>:empty)
  (make-frame type: '<empty-syntax-sequence>))

(define (<syntax-sequence>:empty? seq)
  (eq? '<empty-syntax-sequence> 
       (<frame>:get seq type:)))

(define (<syntax-sequence>:syntax-sequence? seq)
  (let ((stype (<frame>:get seq type:)))
    (or (eq? '<empty-syntax-sequence> stype)
        (eq? '<syntax-sequence> stype))))

(define (<syntax-sequence>:add-first item seq)
  (make-frame type: '<syntax-sequence>
              head: item
              tail: seq))

(define (<syntax-sequence>:sequence . elts)
  (let loop ((elts (reverse elts))
             (result (<syntax-sequence>:empty)))
    (if (null? elts)
        result
        (loop (cdr elts)
              (<syntax-sequence>:add-first (car elts)
                                           result)))))

(define (<syntax-sequence>:make-sequence elts #!key (syntax-type 'sequence))
  (<frame>:put (apply <syntax-sequence>:sequence elts)
               syntax-type: syntax-type))

(define (<syntax-sequence>:first seq)
  (if (<syntax-sequence>:empty? seq)
      (<nothing>:nothing)
      (<frame>:get seq head:)))

(define (<syntax-sequence>:rest seq)
  (if (<syntax-sequence>:empty? seq)
      (<nothing>:nothing)
      (<frame>:get seq tail:)))

(define (<syntax-sequence>:second seq)
  (<syntax-sequence>:first (<syntax-sequence>:rest seq)))

(define (<syntax-sequence>:third seq)
  (<syntax-sequence>:first (<syntax-sequence>:rest (<syntax-sequence>:rest seq))))

(define (<syntax-sequence>:fourth seq)
  (<syntax-sequence>:first (<syntax-sequence>:rest (<syntax-sequence>:rest (<syntax-sequence>:rest seq)))))

(define (<syntax-sequence>:length s)
  (if (<syntax-sequence>:empty? s)
      0
      (+ 1 (<syntax-sequence>:rest s))))

(define (<syntax-sequence>:reverse seq)
  (let loop ((seq seq)
             (result (<syntax-sequence>:empty)))
    (if (<syntax-sequence>:empty? seq)
        result
        (loop (<syntax-sequence>:rest seq)
              (<syntax-sequence>:add-first (<syntax-sequence>:first seq)
                                           result)))))

(define (<syntax-sequence>:append l r)
  (let loop ((ll (<syntax-sequence>:reverse l))
             (result r))
    (if (<syntax-sequence>:empty? ll)
        result
        (loop (<syntax-sequence>:rest ll)
              (<syntax-sequence>:add-first (<syntax-sequence>:first ll)
                                           result)))))

(define (<syntax-sequence>:drop n s)
  (let loop ((n n)
             (result s))
    (if (<= n 0)
        result
        (loop (- n 1)
              (<syntax-sequence>:rest result)))))

(define (<syntax-sequence>:element s n)
  (let loop ((n n)
             (s s))
    (if (<syntax-sequence>:empty? s)
        (error "index out of range" n)
        (if (<= n 0)
            (<syntax-sequence>:first s)
            (loop (- n 1)
                  (<syntax-sequence>:rest seq))))))

(define (<syntax-sequence>:image fn seq)
  (let loop ((seq seq)
             (result (<syntax-sequence>:empty)))
    (if (<syntax-sequence>:empty? seq)
        result
        (loop (<syntax-sequence>:rest seq)
              (<syntax-sequence>:add-first (fn (<syntax-sequence>:first seq))
                                           result)))))


(define (<frame>:format f)
  (let* ((fkeys (<ralist>->list (<frame>:keys f)))
             (fvals (map (lambda (k)(<frame>:get f k)) fkeys)))
        (let loop ((outstr "{")
                   (fkeys fkeys)
                   (fvals fvals))
          (if (null? fkeys)
              (string-append outstr " }")
              (let ((k (car fkeys))
                    (v (car fvals)))
                (loop (string-append outstr
                                     " " (<syntax>:format k)
                                     " " (<syntax>:format v))
                      (cdr fkeys)
                      (cdr fvals)))))))

(define (<syntax-sequence>:format s)
  (let ((stype (<frame>:get s syntax-type:)))
    (let loop ((outstr (string-append "#<" (object->string stype) " ("))
               (s s))
      (if (<syntax-sequence>:empty? s)
          (string-append outstr " )>")
          (loop (string-append outstr " " (<syntax>:format (<syntax-sequence>:first s)))
                (<syntax-sequence>:rest s))))))

(define (<syntax>:format s)
  (if (<frame>:frame? s)
      (if (<syntax-sequence>:syntax-sequence? s)
          (<syntax-sequence>:format s)
          (<frame>:format s))
      (object->string s)))

(define (<syntax>:display s)
  (display (<syntax>:format s)))
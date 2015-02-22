(require <kawa.lib.kawa.expressions>)
(import (kawa lib kawa string-cursors))

;; Optimize (string-cursor-for-each proc str [start [end]])
(define-validate stringCursorForEachValidateApply (exp required proc)
  ((exp:isSimple 2 4)
   (let ((comp (get-compilation))
         (func (exp:getArg 0)))
     (comp:letStart)
     (define seqDecl
       (comp:letVariable #!null string (exp:getArg 1)))
     (define idxDecl
       (comp:letVariable #!null string-cursor
                         (if (> exp:arg-count 2) (exp:getArg 2)
                             (apply-exp as string-cursor 0))))
     (define endDecl
       (comp:letVariable #!null string-cursor
                         (if (> exp:arg-count 3) (exp:getArg 3)
                             (apply-exp string-cursor-end seqDecl))))
     (comp:letEnter)
     (comp:letDone
      (let ((loopLambda (comp:loopStart)))
        (comp:loopEnter)
        (comp:loopDone
         (if-exp (apply-exp string-cursor<? idxDecl endDecl)
                 (begin
                   (comp:letStart)
                   (define chDecl (comp:letVariable #!null character
                                                    (apply-exp
                                                     string-cursor-ref
                                                     seqDecl idxDecl)))
                   (comp:letEnter)
                   (comp:letDone
                    (begin-exp
                     (apply-exp func chDecl)
                     (set-exp idxDecl
                              (apply-exp as string-cursor 
                                         (apply-exp + (apply-exp as int idxDecl)
                                                    (if-exp (apply-exp > (apply-exp as int chDecl) #xFFFF)
                                                            2 1))))
                     (comp:loopRepeat loopLambda)))))))))))

;; Optimize SRFI-13-style (string-for-each proc str [start [end]])
(define-validate stringForEach1ValidateApply (exp required proc)
  ((exp:isSimple 2 2)
   (apply-exp string-cursor-for-each (exp:getArg 0) (exp:getArg 1)))
  ((exp:isSimple 3 4)
   (define comp (get-compilation))
   (comp:letStart)
   (define decl1 (comp:letVariable #!null string (exp:getArg 1)))
   (define decl2 (comp:letVariable #!null int (exp:getArg 2)))
   (comp:letEnter)
   (comp:letDone
    (apply-exp string-cursor-for-each
               (exp:getArg 0) decl1
               (apply-exp string-cursor-next decl1
                          (apply-exp as string-cursor 0)
                          decl2)
               (if (< exp:arg-count 4)
                   (apply-exp string-cursor-end decl1)
                   (apply-exp string-cursor-next decl1
                              (apply-exp as string-cursor 0)
                              (exp:getArg 3)))))))
                   
;; Validate (string-for-each proc str1 [str... | start [end]]
(define-validate stringForEachValidateApply (exp required proc)
  ;; check-for SRFI-13-style (string-for-each proc str start [end])
  ((and (exp:isSimple 3 4)
        (let ((e2 (visit-exp (exp:getArg 2))))
          (exp:setArg 2 e2)
          (let* ((t2 (e2:getType))
                 (integer-compat (invoke integer 'isCompatibleWithValue t2)))
            (or (> integer-compat 0)
                (and (>= integer-compat 0)
                     (< (invoke string 'isCompatibleWithValue t2) 0))))))
   (apply-exp srfi-13-string-for-each (exp:getArg 0) (exp:getArg 1)
              (exp:getArg 2)
              @(if (= exp:arg-count 4) [(exp:getArg 3)] [])))
  ;; R7RS-style (string-for-each proc str1 str...)
  ((and (exp:isSimple 2)
        (or (= exp:arg-count 2)
            (> exp:arg-count 4)
            (let ((e2 (visit-exp (exp:getArg 2))))
              (exp:setArg 2 e2)
              (let* ((t2 (e2:getType))
                     (string-compat (invoke string 'isCompatibleWithValue t2)))
                (or (> string-compat 0)
                    (and (>= string-compat 0)
                         (< (invoke integer 'isCompatibleWithValue t2) 0)))))))
   (let ((n (- (exp:getArgCount) 1))
         (comp (get-compilation))
         (func (exp:getArg 0)))
     (comp:letStart)
     (define seqDecls (gnu.expr.Declaration[] length: n))
     (define idxDecls (gnu.expr.Declaration[] length: n))
     (define endDecls (gnu.expr.Declaration[] length: n))
     (do ((i ::int 0 (+ i 1))) ((= i n))
       (set! (seqDecls i) (comp:letVariable #!null string (exp:getArg (+ i 1))))
       (set! (idxDecls i) (comp:letVariable #!null string-cursor
                                            (apply-exp as string-cursor 0)))
       (set! (endDecls i)  (comp:letVariable #!null string-cursor
                                             (apply-exp invoke (seqDecls i) 'length))))
     (comp:letEnter)
     (comp:letDone
      (let* ((loopLambda (comp:loopStart)))
        (comp:loopEnter)
        (comp:loopDone
         (let loop ((i ::int 0) (chlist '()))
           (cond ((= i n)
                  (begin-exp
                   (apply-exp func @(reverse chlist))
                   (comp:loopRepeat loopLambda)))
                 (else
                  (if-exp
                   (apply-exp string-cursor<? (idxDecls i) (endDecls i))
                   (begin
                    (comp:letStart)
                    (define chDecl
                      (comp:letVariable #!null character
                                        (apply-exp string-cursor-ref
                                                   (seqDecls i) (idxDecls i))))
                    (comp:letEnter)
                    (comp:letDone
                     (begin-exp
                      (set-exp (idxDecls i)
                               (apply-exp as string-cursor
                                          (apply-exp + (apply-exp as int (idxDecls i))
                                                     (if-exp
                                                      (apply-exp > chDecl #xFFFF)
                                                      2 1))))
                      (loop (+ i 1)
                            (cons chDecl chlist)))))))))))))))


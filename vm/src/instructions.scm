;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       Bard
;;;; Purpose:       implementations of vm instructions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "opmacros.scm")

(define %unimplemented (lambda (instr)
                             (error (string-append "unimplemented instruction"
                                                   (object->string instr)))))

;;; 000 machine control
(define %jump  (lambda () (setpc!)))
(define %tjump (lambda () (if (popv!)(setpc!)(incpc!))))
(define %fjump (lambda () (if (popv!)(incpc!)(setpc!))))
(define %cc    (lambda () (error "%cc not implemented yet")(incpc!)))
(define %setcc (lambda () (error "%setcc not implemented yet")(incpc!)))

;;; 032 values
(define %const (lambda () (pushv! (arg1))(incpc!)))
(define %nil   (lambda () (pushv! '())(incpc!)))
(define %zero  (lambda () (pushv! 0)(incpc!)))
(define %one   (lambda () (pushv! 1)(incpc!)))
(define %one-  (lambda () (pushv! -1)(incpc!)))
(define %two   (lambda () (pushv! 2)(incpc!)))

;;; 064 variables
(define %lref  (lambda () (pushv! (lref))(incpc!)))
(define %lset  (lambda () (begin (lset!)(incpc!))))
(define %mref  (lambda () (pushv! (mref))(incpc!)))
(define %mset  (lambda () (begin (mset!)(incpc!))))
(define %modl  (lambda () (error "%modl not implemented yet")(incpc!)))
(define %args  (lambda () (error "%args not implemented yet")(incpc!)))

;;; 096 arithmetic and logic
(define %gt     (lambda () (pushv! (> (popv!)(popv!)))(incpc!)))
(define %gte    (lambda () (pushv! (>= (popv!)(popv!)))(incpc!)))
(define %lt     (lambda () (pushv! (< (popv!)(popv!)))(incpc!)))
(define %lte    (lambda () (pushv! (<= (popv!)(popv!)))(incpc!)))
(define %add    (lambda () (pushv! (+ (popv!)(popv!)))(incpc!)))
(define %sub    (lambda () (pushv! (- (popv!)(popv!)))(incpc!)))
(define %mul    (lambda () (pushv! (* (popv!)(popv!)))(incpc!)))
(define %div    (lambda () (pushv! (/ (popv!)(popv!)))(incpc!)))
(define %rem    (lambda () (pushv! (remainder (popv!)(popv!)))(incpc!)))
(define %expt   (lambda () (pushv! (expt (popv!)(popv!)))(incpc!)))

;;; 128 data manipulations
(define %cons    (lambda () (pushv! (cons (popv!)(popv!)))(incpc!)))
(define %car     (lambda () (pushv! (car (popv!)))(incpc!)))
(define %cdr     (lambda () (pushv! (cdr (popv!)))(incpc!)))
(define %vec     (lambda () (error "%vec not implemented yet")(incpc!)))
(define %vref    (lambda () (error "%vref not implemented yet")(incpc!)))
(define %vset    (lambda () (error "%vset not implemented yet")(incpc!)))
(define %tbl     (lambda () (error "%tbl not implemented yet")(incpc!)))
(define %tblref  (lambda () (error "%tblref not implemented yet")(incpc!)))
(define %tblset  (lambda () (error "%tblset not implemented yet")(incpc!)))

;;; 160 system and I/O
(define %write  (lambda () (error "%write not implemented yet")(incpc!)))
(define %read   (lambda () (error "%read not implemented yet")(incpc!)))
(define %save   (lambda () (error "%save not implemented yet")(incpc!)))
(define %load   (lambda () (error "%load not implemented yet")(incpc!)))
(define %ser    (lambda () (error "%ser not implemented yet")(incpc!)))
(define %dser   (lambda () (error "%dser not implemented yet")(incpc!)))

;;; 192 network and IPC
(define %this  (lambda () (error "%this not implemented yet")(incpc!)))
(define %send  (lambda () (error "%send not implemented yet")(incpc!)))
(define %recv  (lambda () (error "%recv not implemented yet")(incpc!)))

;;; the instructions vector. instructions are stored in an order
;;; defined by the opcodes (see opcodes.scm) so that an opcode is an
;;; index to the location of its corresponding instruction in this
;;; vector. exitfn goes at position 0; it is the HALT continuation
;;; computed at vm startup. %unimplemented occupies the position
;;; corresponding to each unimplemented opcode

(define (instructions-vector exitfn)
  (vector
   
   ;; 000 machine control
   exitfn ; HALT   
   %jump  ; JUMP   
   %tjump ; TJUMP  
   %fjump ; FJUMP  
   %cc    ; CC      
   %setcc ; SETCC   

   ;; 006-031 unimplemented

   %unimplemented ; 006
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 010
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 015
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 020
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 025
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 030
   %unimplemented ; 031

   ;; 032 values
   %const ; CONST  
   %nil   ; NIL    
   %zero  ; ZERO   
   %one   ; ONE    
   %one-  ; ONE-   
   %two   ; TWO    

   ;; 038-063 unimplemented

   %unimplemented ; 038
   %unimplemented ; 
   %unimplemented ; 040
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 045
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 050
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 055
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 060
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 063

   ;; 064 variables
   %lref  ; LREF   
   %lset  ; LSET   
   %mref  ; MREF   
   %mset  ; MSET   
   %modl  ; MODL   
   %args  ; ARGS    

   ;; 070-095 unimplemented

   %unimplemented ; 070
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 075
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 080
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 085
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 090
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 095

   ;; 096 arithmetic and logic
   %gt    ; GT     
   %gte   ; GTE    
   %lt    ; LT     
   %lte   ; LTE    
   %add   ; ADD    
   %sub   ; SUB    
   %mul   ; MUL    
   %div   ; DIV    
   %rem   ; REM    
   %expt  ; EXPT   

   ;; 106-127 unimplemented

   %unimplemented ; 106
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 110
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 115
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 120
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 125
   %unimplemented ; 
   %unimplemented ; 127

   ;; 128 data manipulations
   %cons  ; CONS   
   %car   ; CAR    
   %cdr   ; CDR    
   %vec   ; VEC    
   %vref  ; VREF   
   %vset  ; VSET   
   %tbl   ; TBL    
   %tblref ; TBREF  
   %tblset ; TBSET  

   ;; 137-159 unimplemented

   %unimplemented ; 137
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 140
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 145
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 150
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 155
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 159

   ;; 160 system and I/O
   %write ; WRITE  
   %read  ; READ   
   %save  ; SAVE    
   %load  ; LOAD    
   %ser   ; SER     
   %dser  ; DSER    

   ;; 161-191 unimplemented

   %unimplemented ; 161
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 165
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 170
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 175
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 180
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 185
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 190
   %unimplemented ; 191

   ;; 192 network and IPC
   %this  ; THIS    
   %send  ; SEND   
   %recv  ; RECV   

   ;; 195-255 unimplemented

   %unimplemented ; 195
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 200
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 205
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 210
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 215
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 220
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 225
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 230
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 235
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 240
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 245
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 250
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 
   %unimplemented ; 255

   ))

(cond-expand
 (kawa
  ;; Using 3-operand datum->syntax enables line numbers in reporting.
  (define-syntax xtest
    (lambda (form)
      (syntax-case form ()
        ;; We need to use the rest1 and rest2 variables since the Kawa reader
        ;; currently only attaches line-numbers to pairs, and the quoted and
        ;; evaluated sub-forms aren't guaranteed to be lists.
        ((strtest value . rest1)
         (syntax-case #'rest1 ()
           ((quoted . rest2)
            (syntax-case #'rest2 ()
              ((evaluated)
               #`(begin
             #,(datum->syntax form #'(test-equal quoted (quote value))
                              #'rest1)
             #,(datum->syntax form #'(test-equal evaluated (format "~a" value))
                              #'rest2)))))))))))
 (else
  (define-syntax xtest
    (syntax-rules ()
      ((xtest value quoted evaluated)
       (begin
         (test-equal quoted (quote value))
         (test-equal evaluated (format "~w" value))))))))

(require <kawa.lib.ExceptionClasses>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.prim_syntax>)
(require kawa.lib.prim_imports)
;(import (rename (only (gnu kawa lispexpr LispLanguage) getNamedPartLocation)
;                (getNamedPartLocation $lookup$)))

(define-procedure with-exception-handler
  validate-apply:  "gnu.kawa.functions.CompileMisc:validateApplyWithExceptionHandler"
  (lambda (handler::procedure thunk::procedure)
    name: 'with-exception-handler
    ;; It would be neat to be able to write just 
    ;;    (with-exception-handler handler (lambda () (thunk)))))
    ;; and have validateApplyWithExceptionHandler generate the
    ;; necessary code.  However, we only support calling the
    ;; validate-apply method for already-compiled Procedure values.
    (let ((link (HandlerLink:push handler)))
      (try-catch
       (let ((v (thunk)))
         (link:pop)
         v)
       (ex java.lang.Throwable
           (primitive-throw (link:handle ex)))))))

(define (%%raise form)
  (syntax-case form ()
     ((_ obj)
      #'(primitive-throw (invoke-static kawa.lib.ExceptionWithValue 'wrap obj)))))

(define (raise obj)
  equivalent-syntax: %%raise
  ;; Would be nice to just write: (raise obj) and have that expand %%raise
  (primitive-throw (invoke-static kawa.lib.ExceptionWithValue 'wrap obj)))

(define (raise-continuable obj)
  (let ((save (current-handler:get)))
    (if (eq? save #!null) ;; i.e. current-handler comes from a guard
        (raise obj)
        (try-finally
         (begin
           (current-handler:set save:outer)
           (save:handlerProc obj))
         (current-handler:set save)))))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     (let ((save (current-handler:get)))
       (try-finally
        (try-catch
         (begin
           (current-handler:set #!null)
           e1 e2 ...)
         (ex java.lang.Throwable
             (let ((var (ExceptionWithValue:unwrap ex)))
               (guard-aux
                (primitive-throw ex)
                clause ...))))
        (current-handler:set save))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

;; Guile has: (throw key . args) where key is a symbol.
;; We also support: (throw throwable)
(define (throw #!rest (args ::Object[])) ::never-returns
  (let ((len args:length))
    (if (> len 0)
        (let ((key (args 0)))
          (cond ((symbol? key)
                 (primitive-throw (kawa.lang.NamedException key args)))
                ((and (java.lang.Throwable? key) (= len 1))
                 (gnu.kawa.reflect.Throw:doThrow key)))))
    (primitive-throw (kawa.lang.GenericError "bad arguments to throw"))))
              
;;; The one-argument case is a standard DSSSL procedure.
;;; The multi-argument extension matches Guile.
(define (error #!rest args::Object[])  ::never-returns
  (primitive-throw (kawa.lang.NamedException:makeError @args)))

(define (catch key (thunk :: <procedure>) (handler :: <procedure>))
  (try-catch (thunk)
	     (ex <kawa.lang.NamedException>
                 (invoke ex 'applyHandler key handler))))

(define (error-object? obj) ::boolean
  (instance? obj kawa.lang.NamedException))

(define (error-object-message err::kawa.lang.NamedException)
  (err:getObjectMessage))

(define (error-object-irritants err::kawa.lang.NamedException) ::list
  (err:getObjectIrritants))

(define (read-error? obj) ::boolean
  (instance? obj gnu.text.SyntaxException))

(define (file-error? obj) ::boolean
  (or
   (java.io.FileNotFoundException? obj)
   (cond-expand (java-7
                 (or (java.nio.file.NoSuchFileException? obj)
                     (java.nio.file.InvalidPathException? obj)
                     (java.nio.file.AccessDeniedException? obj)
                     (java.nio.file.DirectoryNotEmptyException? obj)))
                (else
                 (and (java.io.IOException? obj)
                      (((->java.io.IOException obj):getMessage):startsWith
                       "cannot delete"))))))


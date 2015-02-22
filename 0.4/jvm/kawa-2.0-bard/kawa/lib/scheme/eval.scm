(module-name (scheme eval))
(export environment eval)

(require <kawa.lib.std_syntax>)
(require <kawa.lib.lists>)

(with-compile-options
 full-tailcalls: #t
 (define (eval exp #!optional (env ::gnu.mapping.Environment (gnu.mapping.Environment:user)))
   (kawa.lang.Eval:evalForm exp env)))

(define (environment #!rest specifiers)::gnu.mapping.Environment
  (define importer kawa.standard.ImportFromLibrary:instance)
  (define language (kawa.standard.Scheme:getR7rsInstance))
  (define messages (gnu.text.SourceMessages))
  (define lexical (gnu.expr.NameLookup language))
  (define tr (kawa.standard.SchemeCompilation language messages lexical))
  (set! tr:immediate #t)
  (define module (tr:pushNewModule (as java.lang.String #!null)))
  (define env (gnu.mapping.Environment:make (format #f "~{~a~^ ~}" specifiers)))
  (importer:scanForm (cons #f specifiers) module tr)
  (if (messages:seenErrors)
      (primitive-throw (gnu.text.SyntaxException messages)))
  (do ((decl ::gnu.expr.Declaration (module:firstDecl)
             (decl:nextDecl)))
      ((eq? decl #!null))
           (define loc (gnu.kawa.reflect.StaticFieldLocation:make
                        (gnu.expr.Declaration:followAliases decl)))
           (env:addLocation (decl:getSymbol) #!null loc))
  (env:setLocked)
  env)

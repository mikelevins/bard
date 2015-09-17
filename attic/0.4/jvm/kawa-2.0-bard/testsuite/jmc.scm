;; The example in gnu/bytecode/package.html
;; converted to Scheme by Yaroslav Kavenchuk

(define-alias ClassType gnu.bytecode.ClassType)
(define-alias Access gnu.bytecode.Access)
(define-alias Method gnu.bytecode.Method)
(define-alias CodeAttr gnu.bytecode.CodeAttr)
(define-alias Type gnu.bytecode.Type)
(define-alias Variable gnu.bytecode.Variable)
(define-alias ClassTypeWriter gnu.bytecode.ClassTypeWriter)
(define-alias ArrayClassLoader gnu.bytecode.ArrayClassLoader)

(define-alias System java.lang.System)
(define-alias Class java.lang.Class)
(define-alias Integer java.lang.Integer)

;; code

;; "public class HelloWorld extends java.lang.Object".
(define c :: ClassType (ClassType "HelloWorld"))
(*:setSuper c "java.lang.Object")
(*:setModifiers c Access:PUBLIC)


;; "public static int add(int, int)".
(define m :: Method
  (*:addMethod c "add" "(II)I" (logior Access:PUBLIC Access:STATIC)))
(define code :: CodeAttr (m:startCode))
(code:pushScope)
(code:emitLoad (code:getArg 0))
(code:emitLoad (code:getArg 1))
(code:emitAdd Type:intType)
(define resultVar :: Variable (code:addLocal Type:intType "result"))
(code:emitDup)
(code:emitStore resultVar)
(code:emitReturn)
(code:popScope)


;; Get a byte[] representing the class file.
;; We could write this to disk if we wanted.
(define classFile :: byte[] (*:writeToArray c))

;; Disassemble this class.
;; The output is similar to javap(1).
(ClassTypeWriter:print c System:out 0)

;; Load the generated class into this JVM.
;; gnu.bytecode provides ArrayClassLoader, or you can use your own.
(define cl :: ArrayClassLoader (ArrayClassLoader))
(cl:addClass "HelloWorld" classFile)
		
;; Actual invocation is just the usual reflection code.
(define helloWorldClass :: Class (cl:loadClass "HelloWorld" #t))

(define argTypes :: Class[]
  (Class[] java.lang.Integer:TYPE java.lang.Integer:TYPE))

(define result
 (as Integer
     (*:invoke (*:getMethod helloWorldClass "add" argTypes)
               #!null
               (Object[] (Integer 1) (Integer 2)))))
(format #t "Result of HelloWorld.add(1,2) is ~s~%" result)

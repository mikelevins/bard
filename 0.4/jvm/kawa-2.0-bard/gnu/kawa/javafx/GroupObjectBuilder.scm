(define-simple-class GroupObjectBuilder (gnu.kawa.reflect.CompileBuildObject)
  ((hasAddChildMethod) #t)
  ((getChildrenMethod)
   (let ((ct ((getResultType):getName)))
     (cond ((equal? ct "javafx.animation.Timeline") "getKeyFrames")
           ((equal? ct "javafx.scene.shape.Path") "getElements")
           (else "getChildren"))))
  ((buildAddChild target child)
   (gnu.expr.ApplyExp gnu.kawa.reflect.Invoke:invoke
                      (gnu.expr.ApplyExp gnu.kawa.reflect.Invoke:invoke
                                         (gnu.expr.ReferenceExp target)
                                         (gnu.expr.QuoteExp (getChildrenMethod)))
                      (gnu.expr.QuoteExp "add")
                      child)))

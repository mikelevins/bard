(require 'android-defs)

(define-simple-class ViewBuilder (gnu.kawa.reflect.CompileBuildObject)
  (activityType ::gnu.bytecode.ClassType allocation: 'static
            init: (gnu.bytecode.ClassType:make "android.app.Activity"))
  ((hasAddChildMethod) #t)
  ((getAddChildMethodName)
    "addView")
  ((useBuilder numCode::int visitor::gnu.expr.InlineCalls) ::boolean
   (if (or (< (getArgCount) 2)
           (let* ((arg1 (visitor:visit (getArg 1) #!null))
                  (type1 (arg1:getType))             
                  (cmp (invoke activityType 'compare type1)))
             (setArg 1 arg1)
             (< cmp 0)))
       (let ((activity-decl (((getCompilation):getModule):lookup 'current-activity)))
         (insertArgument 1
                         (visitor:visit
                          ((getCompilation):makeCoercion
                           (gnu.expr.ApplyExp (gnu.expr.ReferenceExp activity-decl))
                           android.app.Activity)
#!null))
         #t)
       (invoke-special gnu.kawa.reflect.CompileBuildObject (this) 'useBuilder numCode visitor))
))

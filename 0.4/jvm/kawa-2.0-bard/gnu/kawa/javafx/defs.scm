(define-alias KeyEvent javafx.scene.input.KeyEvent)
(define-alias EventHandler javafx.event.EventHandler)
(define-alias Button javafx.scene.control.Button)
(define-alias Circle javafx.scene.shape.Circle)
(define-alias Rectangle javafx.scene.shape.Rectangle)
(define-alias Color javafx.scene.paint.Color)
(define-alias Text javafx.scene.text.Text)
(define-alias Cursor javafx.scene.Cursor)
(define-alias Group javafx.scene.Group)
(define-alias Node javafx.scene.Node)
(define-alias ObservableList javafx.collections.ObservableList)
(define-alias FXCollections javafx.collections.FXCollections)
(define-alias Timeline javafx.animation.Timeline)
(define-alias KeyFrame javafx.animation.KeyFrame)
(define-alias KeyValue javafx.animation.KeyValue)
(define-alias Path javafx.scene.shape.Path)
(define-alias QuadCurveTo javafx.scene.shape.QuadCurveTo)
(define-alias ArcTo javafx.scene.shape.ArcTo)
(define-alias MoveTo javafx.scene.shape.MoveTo)
(define-alias LineTo javafx.scene.shape.LineTo)
(define-alias HLineTo javafx.scene.shape.HLineTo)
(define-alias VLineTo javafx.scene.shape.VLineTo)
(define-alias Duration javafx.util.Duration)
(define-alias Rotate javafx.scene.transform.Rotate)
(define-alias PerspectiveCamera javafx.scene.PerspectiveCamera)
(define-alias HTMLEditor javafx.scene.web.HTMLEditor)
(define-alias FlowPane javafx.scene.layout.FlowPane)
(define-alias Insets javafx.geometry.Insets)
(define-alias Orientation javafx.geometry.Orientation)

(define-syntax-case javafx-application ()
  ((javafx-application)
   #`(begin
       (module-extends KawaJavafxApplication)
       (define (#,(datum->syntax #'javafx-application 'javafx-stage))::javafx.stage.Stage ;; FIXME
         (this):*stage*))))

(require gnu.kawa.javafx.MakeScene)

(define-syntax-case javafx-scene (javafx-stage)
  ((javafx-scene . args)
   #`(let* ((builder (MakeScene . args))
            (stage (#,(datum->syntax #'javafx-scene 'javafx-stage)))
            (scene (builder:build))
            (title builder:title))
       (invoke (#,(datum->syntax #'javafx-scene 'javafx-stage)) 'setScene scene)
       (if (not (eq? title #!null))
           (set! stage:title title))
       (invoke stage 'show)
       scene)))

(define-simple-class KawaJavafxApplication
  (javafx.application.Application java.lang.Runnable)
  (title ::java.lang.String)
  (*stage* ::javafx.stage.Stage)
  ((run-scene)::void #!void)
  ((run ctx::gnu.mapping.CallContext)::void #!abstract)
  ((run)::void
   (javafx.application.Application:launch ((this):getClass) gnu.expr.ApplicationMainSupport:commandLineArgArray))
  ((runAsMain)::void
   (javafx.application.Application:launch ((this):getClass) gnu.expr.ApplicationMainSupport:commandLineArgArray))
  ((start (stage ::javafx.stage.Stage))::void
   (set! *stage* stage)
   (let ((ctx (gnu.mapping.CallContext:getInstance)))
     ((this):run ctx)
     (ctx:runUntilDone))))

(define-constant {gnu.kawa.reflect/ObjectBuilder}:javafx.animation.Timeline
  "gnu.kawa.javafx.GroupObjectBuilder")
(define-constant {gnu.kawa.reflect/ObjectBuilder}:javafx.scene.shape.Path
  "gnu.kawa.javafx.GroupObjectBuilder")
(define-constant {gnu.kawa.reflect/ObjectBuilder}:javafx.scene.Group
  "gnu.kawa.javafx.GroupObjectBuilder")
(define-constant {gnu.kawa.reflect/ObjectBuilder}:javafx.scene.layout.FlowPane
  "gnu.kawa.javafx.GroupObjectBuilder")


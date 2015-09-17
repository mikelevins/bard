(require <gnu.kawa.android.utils>)
(export Button EditText ImageView LinearLayout ScrollView TextView View
        current-activity activity
        {gnu.kawa.reflect/ObjectBuilder}:android.view.View)

;; FIXME add more
(define-alias Button android.widget.Button)
(define-alias EditText android.widget.EditText)
;(define-alias GridLayout android.widget.GridLayout)
(define-alias ImageView android.widget.ImageView)
(define-alias LinearLayout android.widget.LinearLayout)
(define-alias ScrollView android.widget.ScrollView)
(define-alias TextView android.widget.TextView)
(define-alias View android.view.View)

(define (%process-activity form)
  (syntax-case form (on-create on-create-view)
    (((on-create stmt ...) . rest)
     (cons #`( (onCreate (savedInstanceState :: android.os.Bundle)):: void
	       (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
               (parameterize ((current-activity (this)))
                             stmt ...))
	   (%process-activity #`rest)))
    (((on-create-view stmt ... view) . rest)
     (cons #`( (onCreate (savedInstanceState :: android.os.Bundle)):: void
	       (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
               (parameterize ((current-activity (this)))
                             stmt ...
                             ((this):setContentView view)))
	   (%process-activity #`rest)))
    ((first . rest)
     (cons #`first (%process-activity #`rest)))
    (()
     '())))

(define-syntax-case activity (on-create on-create-view)
  ((activity name . parts)
   #`(define-simple-class name (android.app.Activity)
       #,@(%process-activity #`parts))))

(define-constant {gnu.kawa.reflect/ObjectBuilder}:android.view.View
  "gnu.kawa.android.ViewBuilder")

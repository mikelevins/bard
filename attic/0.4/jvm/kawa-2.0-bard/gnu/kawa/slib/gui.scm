(define-syntax process-keywords
  (syntax-rules ()
		((process-keywords obj args handle-keyword handle-non-keyword)
		 (let ((num-args :: <int> (field args 'length)))
		   (let loop ((i :: <int> 0))
		     (if (< i num-args)
			 (let ((arg ((primitive-array-get <object>) args i)))
			   (cond ((instance? arg <gnu.expr.Keyword>)
				  (handle-keyword obj
						  (gnu.expr.Keyword:getName arg)
						  ((primitive-array-get <object>) args (+ i 1)))
				  (loop (+ i 2)))
				 ((instance? arg <gnu.kawa.xml.KAttr>)
				  (let* ((attr :: <gnu.kawa.xml.KAttr> arg)
					 (name :: <java.lang.String> (invoke attr 'getName))
					 (value (invoke attr 'getObjectValue)))
				    (handle-keyword obj name value))
				  (loop (+ i 1)))
				 (else
				  (handle-non-keyword obj arg)
				  (loop (+ i 1)))))))))))

(define (as-color value) :: <java.awt.Color>
  (cond ((instance? value <java.awt.Color>)
	 value)
	((instance? value <java.lang.Integer>)
	 (make <java.awt.Color> (java.lang.Integer:intValue value)))
	((instance? value <gnu.math.IntNum>)
	 (make <java.awt.Color> (gnu.math.IntNum:intValue value)))
	(else
	 (static-field <java.awt.Color> (*:toString value)))))

(define-private (button-keyword (button :: <gnu.kawa.models.Button>)
				(name :: <java.lang.String>) ; Assumed interned
				value)
  (cond ((eq? name "foreground")
	  (*:setForeground button (as-color value)))
	((eq? name "background")
	 (*:setBackground button (as-color value)))
	;((eq? name 'width) (*:setWidth button (extent value)))
	((eq? name "action")
	 (*:setAction button value))
	;((equal? name "image")
	;#t) ;; not implemented
	((eq? name "text")
	 (*:setText button value))
	((eq? name "disabled")
	 (*:setDisabled button value))	 
	(else (error (format "unknown button attribute ~s" name)))))

(define-private (button-non-keyword (button :: <gnu.kawa.models.Button>)
				    arg)
  #t)

(define (button #!rest args  :: <object[]>)
  (let ((button (make  <gnu.kawa.models.Button>)))
    (process-keywords button args button-keyword button-non-keyword)
    button))

(define (Button #!rest args  :: <object[]>)
  (let ((button (make  <gnu.kawa.models.Button>)))
    (process-keywords button args button-keyword button-non-keyword)
    button))

(define-syntax Image
  (syntax-rules ()
    ((text-field . args)
     (make <gnu.kawa.models.DrawImage> . args))))

(define (image-read (uri :: path)) :: <java.awt.image.BufferedImage>
  (javax.imageio.ImageIO:read (uri:openInputStream)))

(define (image-width (image  :: <java.awt.image.BufferedImage>)) :: <int>
  (*:getWidth image))

(define (image-height (image  :: <java.awt.image.BufferedImage>)) :: <int>
  (*:getHeight image))

(define-private (label-keyword (instance :: <gnu.kawa.models.Label>)
				(name :: <java.lang.String>)
				value)
  (cond ((eq? name 'text)
	  (*:setText instance value))
;	((eq? name 'icon)
;	 (if (or (instance? value <string>)
;		 (instance? value <java.lang.String>))
;	     (set! value
;		   (make <nextapp.echo2.app.ResourceImageReference> value)))
;	 (*:setIcon instance value))
	(else (error (format "unknown label attribute ~s" name)))))

(define-private (label-non-keyword (instance :: <gnu.kawa.models.Label>)
				    arg)
  :: <void>
  (cond ;((instance? arg <gnu.kawa.models.DrawImage>))
	; (*:setIcon instance arg))
	(else
	 (*:setText instance arg))))

(define (Label #!rest args  :: <object[]>)
  (let ((instance (make <gnu.kawa.models.Label>)))
    (process-keywords instance args label-keyword label-non-keyword)
    instance))

(define-private (text-keyword (instance :: <gnu.kawa.models.Text>)
				(name :: <java.lang.String>)
				value)
  (cond ((eq? name 'text)
	  (*:setText instance value))
	;((eq? name 'foreground)
	;  (*:setForeground button value))
	;((eq? name 'background)
	;  (*:setBackground button value))
	;((eq? name 'layout-data)
	;  (*:setLayoutData button value))
	(else (error (format "unknown text attribute ~s" name)))))

(define-private (text-non-keyword (instance :: <gnu.kawa.models.Text>)
				    arg)
  :: <void>
  (*:setText instance arg))

(define (Text #!rest args  :: <object[]>) :: <gnu.kawa.models.Text>
  (let ((instance (make <gnu.kawa.models.Text>)))
    (process-keywords instance args text-keyword text-non-keyword)
    instance))

(define-private (box-keyword (instance :: <gnu.kawa.models.Box>)
				(name :: <java.lang.String>)
				value)
  (cond ;((eq? name 'insets)
;	  (*:setInsets instance value))
	((eq? name 'cell-spacing)
	 (*:setCellSpacing instance value))
	(else (error (format "unknown box attribute ~s" name)))))

(define-private (as-model arg) :: <gnu.kawa.models.Model>
  (invoke (<gnu.kawa.models.Display>:getInstance) 'coerceToModel arg))

(define-private (box-non-keyword (box :: <gnu.kawa.models.Box>)
				    arg) :: <void>
  (*:add box (as-model arg)))
;  (*:add box (as-component arg)))

(define (Row #!rest args  :: <object[]>)
  (let ((instance (make <gnu.kawa.models.Row>)))
    (process-keywords instance args box-keyword box-non-keyword)
    instance))

(define (Column #!rest args  :: <object[]>)
  (let ((instance (make <gnu.kawa.models.Column>)))
    (process-keywords instance args box-keyword box-non-keyword)
    instance))

(define (set-content (window :: <gnu.kawa.models.Window>) pane) :: <void>
  (*:setContent window pane))

(define-private (window-keyword (instance :: <gnu.kawa.models.Window>)
				(name :: <java.lang.String>) ; Assumed interned
				value)
  (cond ((eq? name "title")
	  (*:setTitle instance value))
	((eq? name '"content")
	 (*:setContent instance value))
	((eq? name "menubar")
	 (*:setMenuBar instance value))
	(else (error (format "unknown window attribute ~s" name)))))

(define-private (window-non-keyword (instance :: <gnu.kawa.models.Window>)
				    arg)
  :: <void>
  (*:setContent instance arg))

(define (Window #!rest args  :: <object[]>) :: <gnu.kawa.models.Window>
  (let ((instance
	 (invoke (<gnu.kawa.models.Display>:getInstance) 'makeWindow)))
    (process-keywords instance args window-keyword window-non-keyword)
    instance))

(define-syntax run-application
  (syntax-rules ()
    ((run-application window)
     (gnu.kawa.models.Window:open window))))

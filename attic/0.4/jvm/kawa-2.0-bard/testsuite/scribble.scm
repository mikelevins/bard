;; This is the scribble applet in Flanagan's "Java Examples in a Nutshell"
;; (O'Reilly, 1997), transcribed into Kawa-Scheme.

(define-private last-x 0)
(define-private last-y 0)

(define (init) :: void
  (let ((applet (this)))
    (applet:addMouseListener
     (object (java.awt.event.MouseAdapter)
	     ((mousePressed e)
	      (set! last-x (e:getX))
	      (set! last-y (e:getY)))))
    (applet:addMouseMotionListener
     (object (java.awt.event.MouseMotionAdapter)
	     ((mouseDragged e)
	      (let ((g (applet:getGraphics))
		    (x (e:getX))
		    (y (e:getY)))
		(g:drawLine last-x last-y x y)
		(set! last-x x)
		(set! last-y y)))))))

(define (start) :: void (format #t "called start.~%~!"))
(define (stop) :: void (format #t "called stop.~%~!"))
(define (destroy) :: void (format #t "called destroy.~%~!"))

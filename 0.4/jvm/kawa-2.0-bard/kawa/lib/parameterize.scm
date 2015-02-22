(require <kawa.lib.parameters>)

(define-syntax parameterize%
  (syntax-rules ()
    ((parameterize% () restore . body)
     (try-finally
      (begin . body)
      (begin . restore)))
    ((parameterize% ((param1 value1) . rest) restore . body)
     (let* ((p :: <gnu.mapping.Location> (as-location% param1))
	    (v value1)
	    (save (gnu.mapping.Location:setWithSave p v)))
       (parameterize% rest
		      ((gnu.mapping.Location:setRestore p save) . restore)
		      . body)))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () . body)
     (begin . body))
    ((parameterize ((param1 value1) . rest) . body)
     (parameterize% ((param1 value1) . rest) () . body))))

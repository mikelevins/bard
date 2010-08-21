;;; -*- Mode: Lisp -*-

;;; lambda-list-parsing.lisp --

(in-package "UNIFY")


(declaim (inline type-is-t-p))

(defun type-is-t-p (type-spec)
  (nth-value 0 (and (subtypep type-spec t) (subtypep t type-spec))))


(declaim (inline type-specifier-p))

(defun type-specifier-p (x)
  (nth-value 0 (ignore-errors (subtypep x t))))

(defun normalize-lambda-list (lambda-list)
  (mapcar (lambda (lambda-element)
            (etypecase lambda-element
              (symbol lambda-element)
              (cons (first lambda-element))))
          lambda-list))


(defstruct lambda-var-info
  (name nil :read-only t)
  (type t :read-only t)
  )

(defstruct (optional-lambda-var-info (:include lambda-var-info))
  (default-value nil :read-only t)
  (name-for-supplied-indicator nil :read-only t)
  )

(defstruct (aux-lambda-var-info (:include optional-lambda-var-info)))

(defstruct (key-lambda-var-info (:include optional-lambda-var-info))
  (keyword-name nil :read-only t)
  )

(defstruct (rest-lambda-var-info (:include lambda-var-info)
                                 (:constructor make-rest-lambda-var-info (&key
                                                                          name
                                                                          (type 'list)
                                                                          (element-type t))))
  (element-type t :read-only t)
  )

;;; The next function is really implementation-dependent, give the
;;; definition of LAMBDA-LIST-KEYWORDS 


(define-condition lambda-list-parsing-error (program-error)
  ((item :reader lambda-list-parsing-error-item
         :initarg :item)
   )
  (:report (lambda (llpe stream)
             (format stream "Error while parsing an extended lambda-list (at ~S.)"
                     (lambda-list-parsing-error-item llpe))))
  )


(defun symbol-or-cons-p (x)
  (or (symbolp x) (consp x)))


(defun parse-extended-ordinary-lambda-list (lambda-list
                                            &key
                                            (ordinary-variable-test #'symbolp)
                                            (optional-variable-test #'symbol-or-cons-p)
                                            (rest-variable-test #'symbolp)
                                            (key-variable-test #'symbol-or-cons-p)
                                            (aux-variable-test #'symbol-or-cons-p)
                                            )
  (let ((vars ())
        (optionals ())
        (keywords ())
        (rest ())
        (auxiliary ())
        )
    (labels ((parse-named-arguments (lambda-list &aux (head (first lambda-list)))
               (cond ((null lambda-list) nil)
                     ((and (symbolp head)
                           (member head lambda-list-keywords))
                      (case head
                        (&optional (parse-optional-arguments (rest lambda-list)))
                        (&key (parse-keyword-arguments (rest lambda-list)))
                        (&aux (parse-auxiliary-arguments (rest lambda-list)))
                        (&rest (parse-rest-arguments (rest lambda-list)))
                        (otherwise
                         (warn "Keyword ~A is implementation dependent.~@ 
                                The parsing may not work properly."
                               head)
                         (skip-until-next-lambda-list-keyword (rest lambda-list))
                         ))
                      )
                     ((funcall ordinary-variable-test head)
                      (push head vars)
                      (parse-named-arguments (rest lambda-list)))
                     (t (error 'lambda-list-parsing-error :item head))
                     ))

             (parse-optional-arguments (lambda-list &aux (head (first lambda-list)))
               (cond ((null lambda-list) nil)
                     ((and (symbolp head)
                           (member head lambda-list-keywords))
                      (case head
                        (&optional (error 'lambda-list-parsing-error :item head))
                        (&key (parse-keyword-arguments (rest lambda-list)))
                        (&aux (parse-auxiliary-arguments (rest lambda-list)))
                        (&rest (parse-rest-arguments (rest lambda-list)))
                        (otherwise
                         (warn "Keyword ~A is implementation dependent.~@ 
                                The parsing may not work properly."
                               head)
                         (skip-until-next-lambda-list-keyword (rest lambda-list))
                         ))
                      )
                     ((funcall optional-variable-test head)
                      (push head optionals)
                      (parse-optional-arguments (rest lambda-list)))
                     (t (error 'lambda-list-parsing-error :item head))
                     ))

             (parse-keyword-arguments (lambda-list &aux (head (first lambda-list)))
               (cond ((null lambda-list) nil)
                     ((and (symbolp head)
                           (member head lambda-list-keywords))
                      (case head
                        (&optional (error 'lambda-list-parsing-error :item head))
                        (&key (error 'lambda-list-parsing-error :item head))
                        (&aux (parse-auxiliary-arguments (rest lambda-list)))
                        (&rest (parse-rest-arguments (rest lambda-list)))
                        (&allow-other-keys
                         (unless (or (null (rest lambda-list))
                                     (eql (cadr lambda-list) '&aux))
                           (error 'lambda-list-parsing-error :item head))
                         (skip-until-next-lambda-list-keyword (rest lambda-list)))
                        (otherwise
                         (warn "Keyword ~A is implementation dependent.~@ 
                                The parsing may not work properly."
                               head)
                         (skip-until-next-lambda-list-keyword (rest lambda-list))
                         ))
                      )
                     ((funcall key-variable-test head)
                      (push head keywords)
                      (parse-keyword-arguments (rest lambda-list)))
                     (t (error 'lambda-list-parsing-error :item head))
                     ))

             (parse-rest-arguments (lambda-list &aux (head (first lambda-list)))
               (cond ((null lambda-list) nil)
                     ((consp head)
                      (push head rest)
                      ;; Error checking here.
                      (parse-rest-arguments (rest lambda-list)))
                     ((and (symbolp head)
                           (member head lambda-list-keywords))
                      (case head
                        (&optional (error 'lambda-list-parsing-error :item head))
                        (&key (parse-keyword-arguments (rest lambda-list)))
                        (&aux (parse-auxiliary-arguments (rest lambda-list)))
                        (&rest (error 'lambda-list-parsing-error :item head))
                        (otherwise
                         (warn "Keyword ~A is implementation dependent.~@ 
                                The parsing may not work properly."
                               head)
                         (skip-until-next-lambda-list-keyword (rest lambda-list))
                         ))
                      )
                     ((funcall rest-variable-test head)
                      (push head rest)
                      (parse-rest-arguments (rest lambda-list)))
                     (t (error 'lambda-list-parsing-error :item head))
                     ))

             (parse-auxiliary-arguments (lambda-list &aux (head (first lambda-list)))
               (cond ((null lambda-list) nil)
                     ((and (symbolp head)
                           (member head lambda-list-keywords))
                      (case head
                        (&optional (error 'lambda-list-parsing-error :item head))
                        (&key (error 'lambda-list-parsing-error :item head))
                        (&aux (error 'lambda-list-parsing-error :item head))
                        (&rest (error 'lambda-list-parsing-error :item head))
                        (otherwise
                         (warn "Keyword ~A is implementation dependent.~@ 
                                The parsing may not work properly."
                               head)
                         (skip-until-next-lambda-list-keyword (rest lambda-list))
                         ))
                      )
                     ((funcall aux-variable-test head)
                      (push head auxiliary)
                      (parse-auxiliary-arguments (rest lambda-list)))
                     (t (error 'lambda-list-parsing-error :item head))
                     ))

             (skip-until-next-lambda-list-keyword (lambda-list
                                                   &aux (head (first lambda-list)))
               (cond ((null lambda-list) nil)
                     ((and (symbolp head)
                           (member head lambda-list-keywords))
                      (case head
                        (&optional (parse-optional-arguments (rest lambda-list)))
                        (&key (parse-keyword-arguments (rest lambda-list)))
                        (&aux (parse-auxiliary-arguments (rest lambda-list)))
                        (&rest (parse-rest-arguments (rest lambda-list)))
                        (otherwise
                         (warn "Keyword ~A is implementation dependent.~@ 
                                The parsing may not work properly."
                               head)
                         (skip-until-next-lambda-list-keyword (rest lambda-list))
                         ))
                      )
                     ((symbol-or-cons-p head)
                      (skip-until-next-lambda-list-keyword (rest lambda-list)))
                     ))
             )
      (parse-named-arguments lambda-list)
      (values (nreverse vars)
              (nreverse optionals)
              (nreverse keywords)
              (nreverse rest)
              (nreverse auxiliary))
      )))


(defun parse-var-type-info (var)
  (etypecase var
    (symbol (make-lambda-var-info :name var))
    (cons  (make-lambda-var-info :name (first var) :type (second var)))))


(defun parse-optional-var-type-info (var)
  (etypecase var
    (symbol (make-optional-lambda-var-info :name var))
    (cons (etypecase (first var)
            (symbol (if (second var) ; special case (foo nil) and (foo)
                        (make-optional-lambda-var-info
                         :name (first var)
                         :type (or (and (constantp (second var)) (type-of (second var)))
                                   t))
                        (make-optional-lambda-var-info :name (first var))))
            (cons (make-optional-lambda-var-info
                   :name (caar var)
                   :type (cadar var)
                   :default-value (cadr var)))
            ))
    ))


(defun parse-auxiliary-var-type-info (var)
  (parse-optional-var-type-info var))

(defun parse-rest-var-type-info (var) ; See the FUNCTION type ANSI spec for an explanation.
  (etypecase var
    (symbol (make-rest-lambda-var-info :name var))
    (cons   (make-rest-lambda-var-info :name (first var) :element-type (second var)))))


(defun parse-key-var-type-info (var)
  (etypecase var
    (symbol (make-key-lambda-var-info :name var))
    (cons (destructuring-bind (var &optional (init-value nil init-value-supplied-p))
              var
            (let ((init-value-type
                   (if init-value-supplied-p
                       (or (and (constantp init-value) (type-of init-value))
                           t)
                       t))
                  )
              (etypecase var
                (symbol (make-key-lambda-var-info :name var
                                                  :type init-value-type
                                                  :default-value init-value))
                
                (cons (destructuring-bind (kwd var)
                          var
                        (etypecase var
                          (symbol
                           (make-key-lambda-var-info :name var
                                                     :default-value init-value
                                                     :type init-value-type
                                                     :keyword-name kwd))
                          (cons
                           (make-key-lambda-var-info :name (first var)
                                                     :default-value init-value
                                                     :type (second var)
                                                     :keyword-name kwd))))
                      ))
              )))
    ))


;;; end of file -- lambda-list-parsing.lisp --

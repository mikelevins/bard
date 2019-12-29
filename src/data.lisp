(in-package :bard.internal)

;;; =====================================================================
;;; named constants
;;; =====================================================================

(defclass |undefined| ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun |undefined| ()
  (make-instance '|undefined|))

;;; (|undefined|)

;;; ---------------------------------------------------------------------
;;; booleans
;;; ---------------------------------------------------------------------

(defclass |boolean|()())

(defclass |true| (|boolean|)
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun |true| ()
  (make-instance '|true|))

;;; (|true|)


(defclass |false| (|boolean|)
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun |false| ()
  (make-instance '|false|))

;;; (|false|)

(defmethod true? (thing) t)
(defmethod true? ((thing null)) nil)
(defmethod true? ((thing |false|)) nil)
(defmethod true? ((thing |undefined|)) (error "undefined is neither true nor false"))

(defmethod false? (thing) (not (true? thing)))
(defmethod false? ((thing |undefined|)) (error "undefined is neither true nor false"))

;;; (true? nil)
;;; (true? t)
;;; (true? "Yep")
;;; (true? (|undefined|))
;;; (true? (|true|))
;;; (true? (|false|))
;;; (false? nil)
;;; (false? (|false|))
;;; (false? (|undefined|))

;;; =====================================================================
;;; 
;;; =====================================================================

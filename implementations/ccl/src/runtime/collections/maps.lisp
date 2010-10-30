;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API for pure-functional associative arrays
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.MAPS"
  (:use :as :cl :fn)
  (:import-from :seq "EMPTY?")
  (:nicknames "MAP")
  (:shadow  "GET" "MERGE")
  (:export "ALIST" "ASSOCIATE" "CONTAINS-KEY?" "DISSOCIATE" "GET" "KEYS" 
           "MAKE" "MAKE-AS" "MERGE" "ORDERED-MAP" "PLIST" "VALS" "ZIPMAP"))

(in-package :map)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; the maps subsystem provides a uniform mapping API for alists,
;;; fset:map values, and a simple ordered map implementation that
;;; maintains its key/value pairs in the order they were added.

;;; =====================================================================
;;; private utils
;;; =====================================================================

(defun %plist->alist (plist)
  (if (null plist)
      nil
      (if (null (cdr plist))
          (error "Malformed key/value list in ~S" plist)
          (acons (car plist) (cadr plist)
                 (%plist->alist (cddr plist))))))

(defun %alist->plist (alist)
  (seq:interleave (seq:image 'car alist)
                  (seq:image 'cdr alist)))

;;; =====================================================================
;;; classes
;;; =====================================================================

(defclass alist-map-instance ()
  ((data :reader data :initarg :data)))

(defclass plist-map-instance ()
  ((data :reader data :initarg :data)))

(defclass ordered-map ()
  ((entries :accessor %map-entries :initform nil :initarg :entries)))

(defmethod %find-entry ((om ordered-map) key &key (test 'eql))
  (assoc key (%map-entries om) :test test))

;;; =====================================================================
;;; AS methods
;;; =====================================================================

;;; maps, alists, and plists

(defmethod as ((class (eql 'cl:list)) (thing fset:map) &key &allow-other-keys)
  (error "Use (AS 'map:alist thing) or (AS 'map:plist thing) to convert a map to a list"))

(defmethod as ((class (eql 'cl:list)) (thing ordered-map) &key &allow-other-keys)
  (error "Use (AS 'map:alist thing) or (AS 'map:plist thing) to convert a map to a list"))

(defmethod as ((class (eql 'fset:map)) (thing cl:list) &key &allow-other-keys)
  (error "Use (AS 'fset:map (as 'map:alist thing)) or (AS 'fset:map (as 'map:plist thing)) to convert a list to a map"))

(defmethod as ((class (eql 'ordered-map)) (thing cl:list) &key &allow-other-keys)
  (error "Use (AS 'map:ordered-map (as 'map:alist thing)) or (AS 'map:ordered-map (as 'map:plist thing)) to convert a list to a map"))

(defmethod as ((class (eql 'alist)) (thing fset:map) &key &allow-other-keys)
  (fset:convert 'list thing))

(defmethod as ((class (eql 'plist)) (thing fset:map) &key &allow-other-keys)
  (%alist->plist (fset:convert 'list thing)))

(defmethod as ((class (eql 'alist)) (thing ordered-map) &key &allow-other-keys)
  (%map-entries thing))

(defmethod as ((class (eql 'plist)) (thing ordered-map) &key &allow-other-keys)
  (%alist->plist (%map-entries thing)))

(defmethod as ((class (eql 'alist)) (thing list) &key &allow-other-keys)
  (make-instance 'alist-map-instance :data thing))

(defmethod as ((class (eql 'plist)) (thing list) &key &allow-other-keys)
  (make-instance 'plist-map-instance :data thing))

(defmethod as ((class (eql 'fset:map)) (thing alist-map-instance) &key &allow-other-keys)
  (fset:convert 'fset:map (data thing)))

(defmethod as ((class (eql 'fset:map)) (thing plist-map-instance) &key &allow-other-keys)
  (fset:convert 'fset:map (%plist->alist (data thing))))

(defmethod as ((class (eql 'ordered-map)) (thing alist-map-instance) &key &allow-other-keys)
  (make-instance 'ordered-map :entries (data thing)))

(defmethod as ((class (eql 'ordered-map)) (thing plist-map-instance) &key &allow-other-keys)
  (make-instance 'ordered-map :entries (%plist->alist (data thing))))

;;; maps as sequences

(defmethod as ((class (eql 'sequence)) (thing fset:map) &key &allow-other-keys)
  (seq:zip (keys thing)(vals thing)))

(defmethod as ((class (eql 'sequence)) (thing ordered-map) &key &allow-other-keys)
  (%map-entries thing))

(defmethod as ((class (eql 'sequence)) (thing alist-map-instance) &key &allow-other-keys)
  (data thing))

(defmethod as ((class (eql 'sequence)) (thing plist-map-instance) &key &allow-other-keys)
  (%alist->plist (data thing)))

;;; other conversions

(defmethod as ((class (eql 'fset:map)) (thing fset:map) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'fset:map)) (thing ordered-map) &key &allow-other-keys)
  (as 'fset:map (as 'list thing)))

(defmethod as ((class (eql 'ordered-map)) (thing fset:map) &key &allow-other-keys)
  (as 'ordered-map (as 'list thing)))

(defmethod as ((class (eql 'ordered-map)) (thing ordered-map) &key &allow-other-keys)
  thing)

;;; =====================================================================
;;; MAP API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; associate
;;; ---------------------------------------------------------------------

(defmethod associate ((m cl:list) key val &key (test 'eql) &allow-other-keys)
  (error "Use (AS 'map:alist m) or (AS 'map:plist) to convert lists to maps"))

(defmethod associate ((m alist-map-instance) key val &key (test 'eql) &allow-other-keys)
  (acons key val
         (seq:filter (fn (e)(not ($ test key (car e))))
                     (data m))))

(defmethod associate ((m plist-map-instance) key val &key (test 'eql) &allow-other-keys)
  (let ((pos (position key (data m) :test test)))
    (if pos
        (seq:concat (subseq (data m) 0 pos)
                    (seq:concat (list key val)
                                (subseq (data m) (+ pos 2))))
        (seq:concat (list key val)(data m)))))

(defmethod associate ((m fset:map) key val  &key &allow-other-keys)
  (fset:with m key val))

(defmethod associate ((m ordered-map) key val &key (test 'eql) &allow-other-keys)
  (if (%find-entry m key :test test)
      (make-instance 'ordered-map 
                     :entries (seq:add-last (seq:filter (fn (e)(not ($ test key (car e))))
                                                        (%map-entries m))
                                            (cons key val)))
      (make-instance 'ordered-map :entries (seq:add-last (%map-entries m) (cons key val)))))

;;; ---------------------------------------------------------------------
;;; contains-key?
;;; ---------------------------------------------------------------------

(defmethod contains-key? ((m cl:list) key &key (test 'eql) &allow-other-keys)
  (error "Use (AS 'map:alist m) or (AS 'map:plist) to convert lists to maps"))

(defmethod contains-key? ((m alist-map-instance) key &key (test 'eql) &allow-other-keys)
  (and (assoc key (data m) :test test)
       t))

(defmethod contains-key? ((m plist-map-instance) key &key (test 'eql) &allow-other-keys)
  (and (find key (data m) :test test)
       t))

(defmethod contains-key? ((m fset:map) key &key &allow-other-keys)
  (fset:domain-contains? m key))

(defmethod contains-key? ((m ordered-map) key &key (test 'eql) &allow-other-keys)
  (and (%find-entry m key :test test)
       t))

;;; ---------------------------------------------------------------------
;;; dissociate
;;; ---------------------------------------------------------------------

(defmethod dissociate ((m cl:list) key &key (test 'eql) &allow-other-keys)
  (error "Use (AS 'map:alist m) or (AS 'map:plist) to convert lists to maps"))

(defmethod dissociate ((m alist-map-instance) key &key (test 'eql) &allow-other-keys)
  (remove-if (lambda (entry)
               (funcall test key (car entry))) 
             (data m)))

(defmethod dissociate ((m plist-map-instance) key &key (test 'eql) &allow-other-keys)
  (let ((pos (position key (data m) :test test)))
    (if pos
        (seq:concat (subseq (data m) 0 pos)
                    (subseq (data m) (+ pos 2)))
        (data m))))

(defmethod dissociate ((m fset:map) key &key &allow-other-keys)
  (fset:less m key))

(defmethod dissociate ((m ordered-map) key &key (test 'eql) &allow-other-keys)
  (if (%find-entry m key :test test)
      (make-instance 'ordered-map 
                     :entries (remove-if (lambda (entry) (funcall test key (car entry))) 
                                         (%map-entries m)))
      m))

;;; ---------------------------------------------------------------------
;;; empty?
;;; ---------------------------------------------------------------------

(defmethod empty? ((m alist-map-instance))
  (empty? (data m)))

(defmethod empty? ((m plist-map-instance))
  (empty? (data m)))

(defmethod empty? ((m fset:map))
  (fset:empty? m))

(defmethod empty? ((m ordered-map))
  (empty? (%map-entries m)))

;;; ---------------------------------------------------------------------
;;; get-key
;;; ---------------------------------------------------------------------

(defmethod get ((m alist-map-instance) key &key (test 'eql) (default nil) &allow-other-keys)
  (let ((entry (assoc key (data m) :test test)))
    (if entry (cdr entry) default)))

(defmethod get ((m plist-map-instance) key &key (default nil) &allow-other-keys)
  (getf (data m) key default))

(defmethod get ((m fset:map) key &key (default nil) &allow-other-keys)
  (let ((mdefault (fset:map-default m))
        (found (fset:@ m key)))
    (case (fset:compare mdefault found)
      ((:equal) default)
      (t found))))

(defmethod get ((m ordered-map) key &key (test 'eql) (default nil) &allow-other-keys)
  (let ((entry (%find-entry m key :test test)))
    (if entry (cdr entry) default)))

;;; ---------------------------------------------------------------------
;;; keys
;;; ---------------------------------------------------------------------

(defmethod keys ((m alist-map-instance))(seq:image 'car (data m)))
(defmethod keys ((m plist-map-instance))
  (seq:image (fn (i)(seq:element (data m) i))
             (seq:range 0 (length (data m)) :by 2)))
(defmethod keys ((m fset:map))(fset:domain m))
(defmethod keys ((m ordered-map))(seq:image #'car (%map-entries m)))

;;; ---------------------------------------------------------------------
;;; make
;;; ---------------------------------------------------------------------

(defun make (&rest plist)
  (as 'fset:map (as 'plist plist)))

(defun make-as (type &rest args)
  (case type
    ((alist) (as 'alist (apply 'make args)))
    ((plist) (as 'plist (apply 'make args)))
    ((ordered-map) (make-instance 'ordered-map :entries (as 'alist (apply 'make args))))
    ((fset:map) (apply 'make args))
    (t (error "Unrecognized map type: ~S" type))))

;;; ---------------------------------------------------------------------
;;; merge
;;; ---------------------------------------------------------------------

(defmethod merge ((m1 alist-map-instance) m2 &key (test 'eql) &allow-other-keys)
  (if (empty? m2)
      (data m1)
      (as 'alist
          (merge (as 'fset:map m1)
                 (as 'fset:map m2)))))

(defmethod merge ((m1 plist-map-instance) m2 &key (test 'eql) &allow-other-keys)
  (if (empty? m2)
      (data m1)
      (as 'plist
          (merge (as 'fset:map m1)
                 (as 'fset:map m2)))))

(defmethod merge ((m1 fset:map) m2 &key &allow-other-keys)
  (if (empty? m2)
      m1
      (fset:map-union m1 (as 'fset:map m2))))

(defmethod merge ((m1 ordered-map) m2 &key (test 'eql) &allow-other-keys)
  (if (empty? m2)
      m1
      (let* ((m2-pairs (as 'list m2))
             (m1-pairs (seq:filter (fn (e) (not (seq:some? (fn (p) ($ test (car e)(car p)))
                                                           m2-pairs)))
                                   (%map-entries m1))))
        (make-instance 'ordered-map :entries (seq:concat m1-pairs m2-pairs)))))

;;; ---------------------------------------------------------------------
;;; vals
;;; ---------------------------------------------------------------------

(defmethod vals ((m alist-map-instance))(seq:image 'cdr (data m)))
(defmethod vals ((m plist-map-instance))
  (seq:image (fn (i)(seq:element (data m) i))
             (seq:range 1 (length (data m)) :by 2)))
(defmethod vals ((m fset:map))(fset:range m))
(defmethod vals ((m ordered-map))(seq:image 'cdr (%map-entries m)))

;;; ---------------------------------------------------------------------
;;; zipmap
;;; ---------------------------------------------------------------------

(defmethod zipmap ((ks cl:list)(vs cl:list))
  (as 'fset:map (as 'alist (seq:image (lambda (k v) (cons k v)) ks vs))))

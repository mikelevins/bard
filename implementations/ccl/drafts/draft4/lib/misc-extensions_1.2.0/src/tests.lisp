;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GMap -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package gmap)

;;; Itty bitty test suite -- also serves as a demo.
(unless (and (equalp (gmap (:list :filterp #'oddp) #'+ (:index 0 6) (:constant 17))
		     '(17 19 21))
	     (equalp (gmap :alist #'(lambda (x y) (values x (* x y)))
			   (:plist '(2 14 3 26 -4 19)))
		     '((2 . 28) (3 . 78) (-4 . -76)))
	     (equalp (gmap :vector nil (:index 11 1 :incr -3))
		     '#(8 5 2))
	     (equalp (gmap (:vector :use-vector (make-array 1 :fill-pointer 0
							    :adjustable t)
				    :fill-pointer t :adjustable t)
			   #'+
			   (:simple-vector '#(3 44 217))
			   (:index-inc 17 19))
		     '#(20 62 236))
	     (equalp (gmap (:vector :use-vector (make-array 3))
			   #'+
			   (:vector '#(33 44 55))
			   (:index-inc 22 13 :incr -4))
		     '#(55 62 69))
	     (equalp (gmap :string #'code-char (:index 32 119 :incr 12))
		     " ,8DP\\ht")
	     (equalp (gmap (:string :use-string (make-array 1 :element-type 'character
							    :fill-pointer 0 :adjustable t)
				    :fill-pointer t :adjustable t)
			   #'code-char (:index-inc 33 105 :incr 12))
		     "!-9EQ]i")
	     (equalp (gmap (:string :use-string (make-string 4)) #'code-char
			   (:index 70 74))
		     "FGHI")
	     (equalp (gmap :append nil (:list '((seoie 2dlkes) (zlcxildk oiden xinld)
						(kthsio soi3kd zilk) (oiwnxlk lkdw))))
		     '(seoie 2dlkes zlcxildk oiden xinld kthsio soi3kd zilk oiwnxlk lkdw))
	     ;; Fun with multiple values!
	     (equalp (multiple-value-list (gmap (:values :list :vector) nil
						(:alist '((x . 1) (y . 2) (z . 3)))))
		     '((x y z) #(1 2 3)))
	     (equalp (gmap :alist nil (:plist '(a 17 b 28 q 47)))
		     '((a . 17) (b . 28) (q . 47)))
	     (equalp (gmap :plist (lambda (x y) (values (string x) (float y)))
			   (:alist '((X . 2) (Y . 13) (Z . 44))))
		     '("X" 2.0 "Y" 13.0 "Z" 44.0))
	     ;; Shows how multiple-value-consuming result-specs interact with `:values'.
	     (equalp (multiple-value-list
			 (gmap (:values :alist :vector)
			       (lambda (x y) (values x y (1+ y)))
			       (:plist '(x 1 y 2 z 3))))
		     '(((x . 1) (y . 2) (z . 3)) #(2 3 4))))
  (error "Some test case failed"))


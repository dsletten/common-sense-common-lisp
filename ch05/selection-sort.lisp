;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               selection-sort.lisp
;;;;
;;;;   Started:            Tue Jul 15 16:55:12 2025
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :selection-sort (:use :common-lisp :core :test))

(in-package :selection-sort)

;; (defun selection-sort (a test &key (key #'identity))
;; ;  (loop for i from 0 below (length a)
;;   (loop for i from 0 below (1- (length a))
;;         for extreme = (aref a i)
;;         for index = i
;;         do (loop for j from (1+ i) below (length a)
;;                  if (funcall test (aref a j) extreme)
;;                  do (setf extreme (aref a j)
;;                           index j))
;;         unless (= index i)
;;         do (rotatef (aref a i) (aref a index))
;;         finally (return a)))

(defun should-swap-p (a b test &optional key)
  (if (functionp key)
      (funcall test (funcall key a) (funcall key b))
      (funcall test a b)))

(defun selection-sort-1 (a test &key key)
  (loop for i from 0 below (1- (length a))
        for extreme = (aref a i)
        for index = i
        do (loop for j from (1+ i) below (length a)
                 if (should-swap-p (aref a j) extreme test key)
                 do (setf extreme (aref a j)
                          index j))
        unless (= index i)
        do (rotatef (aref a i) (aref a index))
        finally (return a)))

(defun selection-sort-2 (a test &key key)
  (labels ((outer (i)
             (if (>= i (1- (length a)))
                 a
                 (let ((index (inner (1+ i) i)))
                   (unless (= index i)
                     (rotatef (aref a i) (aref a index)))
                   (outer (1+ i)))) )
           (inner (j index)
             (cond ((= j (length a)) index)
                   ((should-swap-p (aref a j) (aref a index) test key)
                    (inner (1+ j) j))
                   (t (inner (1+ j) index)))) )
    (outer 0)))
                        
(defun selection-sort-list-1 (l test &key key)
  (loop for i on l
        until (null (rest i))
        for index = i
        do (loop for j on (rest i)
                 if (should-swap-p (first j) (first index) test key)
                 do (setf index j))
        unless (eq index i)
        do (rotatef (first index) (first i))
        finally (return l)))

(defun selection-sort-list-2 (l test &key key)
  (labels ((outer (i)
             (if (null (rest i))
                 l
                 (let ((index (inner (rest i) i)))
                   (unless (eq index i)
                     (rotatef (first index) (first i)))
                   (outer (rest i)))) )
           (inner (j index)
             (cond ((null j) index)
                   ((should-swap-p (first j) (first index) test key)
                    (inner (rest j) j))
                   (t (inner (rest j) index)))) )
    (outer l)))

(defun random-ordered-list ()
  (let* ((random-state (make-random-state t))
         (low (random 200 random-state))
         (high (+ low (random 100 random-state))))
    (loop for i from low upto high collect i)))

(defun random-ordered-array ()
  (coerce (random-ordered-list) 'vector))

;(test-selection-sort #'selection-sort-list-5 'list)

(deftest test-selection-sort (selection-sort type)
  (check
   (eqls (apply type '())
         (funcall selection-sort (apply type '()) #'<))
   (eqls (apply type '(10))
         (funcall selection-sort (apply type '(10)) #'<))
   (eqls (apply type '(10 20))
         (funcall selection-sort (apply type '(10 20)) #'<))
   (eqls (apply type '(10 20))
         (funcall selection-sort (apply type '(20 10)) #'<))
   ;;;
   ;;;    Not stable
   ;;;    
   ;; (eqls (apply type '(1 1d0 2 3d0 3))
   ;;       (funcall selection-sort (apply type '(3d0 1 2 3 1d0)) #'<)) ; Stable
   (eqls (apply type '(3d0 3 2 1 1d0))
         (funcall selection-sort (apply type '(3d0 1 2 3 1d0)) #'>)) ; Stable - iffy
   (eqls (apply type '(2 2 4 4 6 6 8 8))
         (funcall selection-sort (apply type '(2 4 6 8 2 4 6 8)) #'<))
   (eqls (apply type '(8 8 6 6 4 4 2 2))
         (funcall selection-sort (apply type '(2 4 6 8 2 4 6 8)) #'>))
   (eqls (apply type '(1 2 3 4 5))
         (funcall selection-sort (apply type '(1 2 3 4 5)) #'<))
   (eqls (apply type '(1 2 3 4 5))
         (funcall selection-sort (apply type '(5 4 3 2 1)) #'<))
   (eqls (apply type '(5 4 3 2 1))
         (funcall selection-sort (apply type '(1 2 3 4 5)) #'>))
   (eqls (apply type '(5 4 3 2 1))
         (funcall selection-sort (apply type '(5 4 3 2 1)) #'>))
   (eqls (apply type '(1/5 1/4 1/3 1/2 1))
         (funcall selection-sort (apply type '(1 1/3 1/5 1/2 1/4)) #'<))
   (eqls (apply type '(1 1/2 1/3 1/4 1/5))
         (funcall selection-sort (apply type '(1 1/3 1/5 1/2 1/4)) #'>))
   (eqls (apply type '(1 1/2 1/3 1/4 1/5))
         (funcall selection-sort (apply type '(1 1/3 1/5 1/2 1/4))
                  #'(lambda (m n) (< (denominator m) (denominator n)))) )
   (eqls (apply type '(1 1/2 1/3 1/4 1/5))
         (funcall selection-sort (apply type '(1 1/3 1/5 1/2 1/4)) #'< :key #'denominator))
   (let ((a (apply type '(61 82 67 4 98 20 37 85))))
     (eqls (apply type '(4 20 37 61 67 82 85 98)) (funcall selection-sort a #'<)))
   (eqls (apply type '("pung" "foo" "baz" "bar"))
         (funcall selection-sort (apply type '("pung" "foo" "bar" "baz")) #'string>))
   (eqls (apply type '("Pung" "FOO" "baz" "BAR"))
         (funcall selection-sort (apply type '("Pung" "FOO" "BAR" "baz")) #'string-greaterp))
   (eqls (apply type '("Pung" "FOO" "baz" "BAR"))
         (funcall selection-sort (apply type '("Pung" "FOO" "BAR" "baz")) #'string> :key #'string-downcase))
   (eqls (apply type '("bar" "baz" "foo" "pung"))
         (funcall selection-sort (apply type '("pung" "foo" "bar" "baz")) #'string<))
   (eqls (apply type '("BAR" "baz" "Foo" "pUNG"))
         (funcall selection-sort (apply type '("pUNG" "Foo" "BAR" "baz")) #'string-lessp))
   ;;;
   ;;;    Not stable - these are iffy
   ;;;    
   (eqls (apply type '("foo" "bar" "baz"))
         (funcall selection-sort (apply type '("foo" "bar" "baz")) #'(lambda (a b) (< (length a) (length b)))) ) ; Stable
   (eqls (apply type '("foo" "bar" "baz"))
         (funcall selection-sort (apply type '("foo" "bar" "baz")) #'< :key #'length)) ; Stable
   (eqls (apply type '("foo" "bar" "baz"))
         (funcall selection-sort (apply type '("foo" "bar" "baz")) #'(lambda (a b) (> (length a) (length b)))) ) ; Stable
   (eqls (apply type '("foo" "bar" "baz" "cat" "dog" "car" "cdr" "pung"))
         (funcall selection-sort (apply type '("pung" "foo" "bar" "baz" "cat" "dog" "car" "cdr")) #'(lambda (a b) (< (length a) (length b)))) ) ; Stable
   (eqls (apply type '((z . 2) (k . 3) (p . 4) (a . 5) (b . 9)))
         (funcall selection-sort (coerce '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)) type)
                  #'(lambda (a b) (< (cdr a) (cdr b)))) )
   (eqls (apply type '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)))
         (funcall selection-sort (coerce '((B . 9) (A . 5) (K . 3) (P . 4) (Z . 2)) type)
                  #'(lambda (a b) (string< (string (car a)) (string (car b)))) ))
   (eqls (apply type '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)))
         (funcall selection-sort (coerce '((B . 9) (A . 5) (K . 3) (P . 4) (Z . 2)) type)
                  #'string< :key (compose #'string #'car)))
   (dotimes (i 100 t)
     (let* ((a (case type
                 (vector (random-ordered-array))
                 (list (random-ordered-list))))
            (b (coerce (shuffle (coerce (copy-seq a) 'vector)) type)))
       (unless (and (eqls a (funcall selection-sort b #'<))
                    (eqls (reverse a) (funcall selection-sort b #'>)))
         (return nil)))) ))

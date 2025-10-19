;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               insertion-sort.lisp
;;;;
;;;;   Started:            Wed Aug  6 17:38:54 2025
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
;;;;   The book's description hides the clarity of the algorithm!
;;;;   Consider an array that is already sorted. To insert a new element
;;;;   at the correct location, use binary search to locate the proper
;;;;   "hypothetical" location of the element, then shift the array from
;;;;   that point to the right to allow room for the new element.
;;;;
;;;;   见 ordered-array in ch. 2!!!
;;;;
;;;;   The book's algorithm does this in place. Assume that initially the
;;;;   array consists of the first element as a single-element sorted array.
;;;;   Then iterate over each remaining elt as though you were "inserting" it
;;;;   into the subsequence that is already sorted.
;;;;
;;;;   The conventional insertion sort algorithm expressed in the book (and by Kubica)
;;;;   performs a linear search in the inner loop looking for the correct location of
;;;;   the current elt designated by the outer loop. But there is a significant secondary
;;;;   purpose, as the inner loop descends it shifts existing elements over to make room
;;;;   for the elt currently being "inserted".  So despite the slower performance of
;;;;   this linear traversal it kills two birds with one stone.
;;;;
;;;;   My version below finds the correct position faster via binary search, but there remains
;;;;   the work of shifting array elements over to accomodate the insertion. This sort
;;;;   of bulk memory shift should be doable quite efficiently at the hardware level. The
;;;;   crucial question is whether or not the programming language supports this. If the
;;;;   language does not, the conventional algorithm might be quicker. (Which is what the
;;;;   timings below seem to suggest!!)
;;;;
;;;;   In other words, SETF on SUBSEQ is _not_ the bulk operation I was expecting! It
;;;;   creates an ephemeral copy of a subsequence, which explains all of the CONSing in
;;;;   the timing below!) But REPLACE is???
;;;;   
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :insertion-sort
  (:use :common-lisp :core :test))

(in-package :insertion-sort)

;;;
;;;    Does not allow duplicates.
;;;    - Insert in place
;;;    - Array does not grow
;;;    
(defun insert (a n obj test)
  "Insert OBJ into array A whose first N elements are ordered by TEST."
  (labels ((shift-up (i)
             (setf (subseq a (1+ i) (1+ n)) (subseq a i))) ; This creates an ephemeral copy!
           (insert-at (i)
;             (assert (<= 0 i n) () "Invalid index: ~A" index)
             (shift-up i)
             (setf (aref a i) obj)))
    (let ((index (binary-search a obj test :end (1- n))))
      (when (minusp index)
        (insert-at (1- (- index)))) )))

(defun insertion-sort (a test)
  (loop for i from 1 below (length a)
        do (insert a i (aref a i) test)))

;;;
;;;    This handles duplicates but isn't stable:
;;;
;; (setf *a* (vector 9 0 2 7 4 2d0 2 2d0 2f0 2))
;; #(9 0 2 7 4 2.0d0 2 2.0d0 2.0 2)
;; * (insertion-sort** *a* #'<)
;; NIL
;; * *a*
;; #(0 2 2.0d0 2 2.0 2 2.0d0 4 7 9)
(defun insert** (a n obj test)
  "Insert OBJ into array A whose first N elements are ordered by TEST."
  (labels ((shift-up (i)
             (replace a a :start1 (1+ i) :end1 (1+ n) :start2 i))
           (insert-at (i)
;             (assert (<= 0 i n) () "Invalid index: ~A" index)
             (shift-up i)
             (setf (aref a i) obj)))
    ;; (let ((index (binary-search a obj test :end (1- n))))
    ;;   (when (minusp index)
    ;;     (insert-at (1- (- index)))) )))
    (let ((index (binary-search a obj test :end (1- n))))
      (if (minusp index)
          (insert-at (1- (- index)))
          (insert-at (1+ index)))) ))

(defun insertion-sort** (a &key (test #'<))
  (loop for i from 1 below (length a)
        do (insert** a i (aref a i) test)
        finally (return a)))

;;;
;;;    In the timings below, this is INSERTION-SORT:
;;;    见 ~/lisp/books/Kubica/ch01/ch01.lisp
;;;
;; (defun insertion-sort (a &key (test #'<))
;;   (do ((n (length a))
;;        (i 1 (1+ i)))
;;       ((>= i n) a)
;;     (do ((current (aref a i))
;;          (j (1- i) (1- j)))
;;         ((or (minusp j) (not (funcall test current (aref a j))))
;;          (setf (aref a (1+ j)) current))
;;       (setf (aref a (1+ j)) (aref a j)))) )

;;;
;;;    INSERTION-SORT* is INSERTION-SORT above in this file (using INSERT).
;;;    

;; (let* ((a (random-ordered-array)) (b (copy-seq a)) (c (copy-seq a))) (time (loop repeat 1000 do (insertion-sort b))) (time (loop repeat 1000 do (insertion-sort* c #'<))))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000216 seconds of total run time (0.000216 user, 0.000000 system)
;;   100.00% CPU
;;   686,669 processor cycles
;;   0 bytes consed
  
;; Evaluation took:
;;   0.002 seconds of real time
;;   0.001592 seconds of total run time (0.001592 user, 0.000000 system)
;;   100.00% CPU
;;   5,070,460 processor cycles
;;   2,487,888 bytes consed
  
;; NIL

;;;
;;;    INSERTION-SORT** using REPLACE
;;;
;; Evaluation took:
;;   0.004 seconds of real time
;;   0.004178 seconds of total run time (0.004178 user, 0.000000 system)
;;   100.00% CPU
;;   13,313,666 processor cycles
;;   0 bytes consed




(defun random-ordered-array ()
  (let* ((random-state (make-random-state t))
         (low (random 200 random-state))
         (high (+ low (random 100 random-state))))
    (apply #'vector (loop for i from low upto high collect i))))

(deftest test-insertion-sort** ()
  (check
   (eqls #() (insertion-sort** (vector)))
   (eqls #(10) (insertion-sort** (vector 10)))
   (eqls #(10 20) (insertion-sort** (vector 10 20)))
   (eqls #(10 20) (insertion-sort** (vector 20 10)))
   (eqls #(1 1d0 2 3d0 3) (insertion-sort** (vector 3d0 1 2 3 1d0))) ; Stable
   (eqls #(3d0 3 2 1 1d0) (insertion-sort** (vector 3d0 1 2 3 1d0) :test #'>)) ; Stable
   (eqls #(2 2 4 4 6 6 8 8) (insertion-sort** (vector 2 4 6 8 2 4 6 8)))
   (eqls #(8 8 6 6 4 4 2 2) (insertion-sort** (vector 2 4 6 8 2 4 6 8) :test #'>))
   (eqls #(1 2 3 4 5) (insertion-sort** (vector 1 2 3 4 5)))
   (eqls #(1 2 3 4 5) (insertion-sort** (vector 5 4 3 2 1)))
   (eqls #(5 4 3 2 1) (insertion-sort** (vector 1 2 3 4 5) :test #'>))
   (eqls #(5 4 3 2 1) (insertion-sort** (vector 5 4 3 2 1) :test #'>))
   (eqls #(1/5 1/4 1/3 1/2 1) (insertion-sort** (vector 1 1/3 1/5 1/2 1/4)))
   (eqls #(1 1/2 1/3 1/4 1/5) (insertion-sort** (vector 1 1/3 1/5 1/2 1/4) :test #'>))
   (eqls #(1 1/2 1/3 1/4 1/5) (insertion-sort** (vector 1 1/3 1/5 1/2 1/4)
                                                :test #'(lambda (m n) (< (denominator m)
                                                                         (denominator n)))) )
   (let ((a (vector 61 82 67 4 98 20 37 85)))
     (eqls #(4 20 37 61 67 82 85 98) (insertion-sort** a)))
   (eqls #("pung" "foo" "baz" "bar")
         (insertion-sort** (vector "pung" "foo" "bar" "baz") :test #'string>))
   (eqls #("Pung" "FOO" "baz" "BAR")
         (insertion-sort** (vector "Pung" "FOO" "BAR" "baz") :test #'string-greaterp))
   (eqls #("bar" "baz" "foo" "pung")
         (insertion-sort** (vector "pung" "foo" "bar" "baz") :test #'string<))
   (eqls #("BAR" "baz" "Foo" "pUNG")
         (insertion-sort** (vector "pUNG" "Foo" "BAR" "baz") :test #'string-lessp))
   (eqls #("foo" "bar" "baz")
         (insertion-sort** (vector "foo" "bar" "baz") :test #'(lambda (a b) (< (length a) (length b)))) ) ; Stable XXXXX
   (eqls #("foo" "bar" "baz")
         (insertion-sort** (vector "foo" "bar" "baz") :test #'(lambda (a b) (> (length a) (length b)))) ) ; Stable XXXXX
   (eqls #("foo" "bar" "baz" "cat" "dog" "car" "cdr" "pung")
         (insertion-sort** (vector "pung" "foo" "bar" "baz" "cat" "dog" "car" "cdr") :test #'(lambda (a b) (< (length a) (length b)))) ) ; Stable XXXXX
   (eqls #((z . 2) (k . 3) (p . 4) (a . 5) (b . 9))
           (insertion-sort** (coerce '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)) 'vector)
                           :test #'(lambda (a b) (< (cdr a) (cdr b)))) )
   (eqls #((a . 5) (b . 9) (k . 3) (p . 4) (z . 2))
           (insertion-sort** (coerce '((B . 9) (A . 5) (K . 3) (P . 4) (Z . 2)) 'vector)
                           :test #'(lambda (a b) (string< (string (car a)) (string (car b)))) ))
   (dotimes (i 100 t)
     (let* ((a (random-ordered-array))
            (b (shuffle (copy-seq a))))
       (unless (eqls a (insertion-sort** b))
         (return nil)))) ))

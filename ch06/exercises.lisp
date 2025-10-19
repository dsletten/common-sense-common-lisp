;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    In Lisp there is always more than one way to solve a problem.
;;;;    -- David Touretzky
;;;;
;;;;    Name:               exercises.lisp
;;;;
;;;;    Started:            Sat Aug 30 10:44:31 2025
;;;;    Modifications:
;;;;
;;;;    Purpose:
;;;;
;;;;
;;;;
;;;;    Calling Sequence:
;;;;
;;;;
;;;;    Inputs:
;;;;
;;;;    Outputs:
;;;;
;;;;    Example:
;;;;
;;;;    Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :exercises
  (:use :common-lisp :core :test)
  (:shadow :intersection))

(in-package :exercises)

(defun intersection (a b)
  (loop with h = (make-hash-table)
        for elt in a
        do (setf (gethash elt h) t)
        finally (return (loop for elt in b
                              when (gethash elt h)
                              collect elt))))

;;;
;;;    3. Determine whether any pair of numbers in an array A add up to 10.       
;;;    (A number cannot be paired with itself, but it can be paired with          
;;;     another equal element, e.g. two 5's.)                                     
;;;                                                                               
;;;    The book examines the cartesian product of the array elts with themselves, 
;;;    potentially a quadratic process in the worst case of no match.             
;;;                                                                               
;;;    My version does one preparatory pass (O(N) best case) that records:        
;;;    - The missing sum for each elt needed to yield 10                          
;;;    - A reverse index of existing elts to their array index                    
;;;                                                                               
;;;    The 2nd pass examines each delta to see if that elt exists in the array at 
;;;    a different index.
;;;
(defun two-sum (a &optional (sum 10))
  (loop with deltas = (make-array (length a))
        with as = (make-hash-table)
        for elt across a
        for i from 0
        do (setf (gethash elt as) i)  ; Duplicate elts?????????
        do (setf (aref deltas i) (- sum elt))
        finally (loop for delta across deltas
                      for i from 0
                      for hit = (gethash delta as)
                      if (and hit (/= hit i))
                      do (return-from two-sum (list (aref a i) delta)))) )

(defun two-sum (a &optional (sum 10))
  (loop with deltas = (make-array (length a))
        with as = (make-hash-table)
        for elt across a
        for i from 0
        for indexes = (gethash elt as (make-array 10 :fill-pointer 0 :adjustable t))
        do (vector-push-extend i indexes)
           (setf (gethash elt as) indexes
                 (aref deltas i) (- sum elt))
        finally (loop for delta across deltas
                      for i from 0
                      for hit = (gethash delta as)
                      if (and hit (notevery (partial #'= i) hit))
                      do (return-from two-sum (list (aref a i) delta)))) )

(defun two-sum (a &optional (sum 10))
  (let* ((n (length a))
         (deltas (make-array n))
         (as (make-hash-table)))
    (do ((i 0 (1+ i)))
        ((= i n)
         (do ((i 0 (1+ i)))
             ((= i n) nil)
           (let* ((delta (aref deltas i))
                  (hit (gethash delta as)))
             (when (and hit (notevery (partial #'= i) hit))
               (return (list (aref a i) delta)))) ))
      (let* ((elt (aref a i))
             (indexes (gethash elt as (make-array 10 :fill-pointer 0 :adjustable t))))
        (vector-push-extend i indexes)
        (setf (gethash elt as) indexes
              (aref deltas i) (- sum elt)))) ))

(defun two-sum (a &optional (sum 10))
  (labels ((get-bin-for-elt (elt h)
             (gethash elt h (make-array 10 :fill-pointer 0 :adjustable t))))
    (let* ((n (length a))
           (deltas (make-array n))
           (as (make-hash-table)))
      (do ((i 0 (1+ i)))
          ((= i n)
           (do ((i 0 (1+ i)))
               ((= i n) nil)
             (let* ((delta (aref deltas i))
                    (hit (gethash delta as)))
               (when (and hit (notevery (partial #'= i) hit))
                 (return (list (aref a i) delta)))) ))
        (let* ((elt (aref a i))
               (bin (get-bin-for-elt elt as)))
          (vector-push-extend i bin)
          (setf (gethash elt as) bin
                (aref deltas i) (- sum elt)))) )))

(defun two-sum (a &optional (sum 10))
  (labels ((get-bin-for-elt (elt h)
             (gethash elt h (make-array 10 :fill-pointer 0 :adjustable t))))
    (let* ((n (length a))
           (deltas (make-array n))
           (as (make-hash-table)))
      (dotimes (i n)
        (let* ((elt (aref a i))
               (bin (get-bin-for-elt elt as)))
          (vector-push-extend i bin)
          (setf (gethash elt as) bin
                (aref deltas i) (- sum elt))))
      (dotimes (i n nil)
        (let* ((delta (aref deltas i))
               (hit (gethash delta as)))
          (when (and hit (notevery (partial #'= i) hit))
            (return (list (aref a i) delta)))) ))))

(defun two-sum (a &optional (sum 10))
  (let ((n (length a)))
    (labels ((get-bin-for-elt (elt h)
               (gethash elt h (make-array 10 :fill-pointer 0 :adjustable t)))
             (scan (i deltas as)
               (if (= i n)
                   (seek 0 deltas as)
                   (let* ((elt (aref a i))
                          (bin (get-bin-for-elt elt as)))
                     (vector-push-extend i bin)
                     (setf (gethash elt as) bin
                           (aref deltas i) (- sum elt))
                     (scan (1+ i) deltas as))))
             (seek (i deltas as)
               (if (= i n)
                   nil
                   (let* ((delta (aref deltas i))
                          (hit (gethash delta as)))
                     (if (and hit (notevery (partial #'= i) hit))
                         (list (aref a i) delta)
                         (seek (1+ i) deltas as)))) ))
      (scan 0 (make-array n) (make-hash-table)))) )

(defun two-sum (a &optional (sum 10))
  (let* ((n (length a))
         (deltas (make-array n))
         (as (make-hash-table)))
    (labels ((get-bin-for-elt (elt h)
               (gethash elt h (make-array 10 :fill-pointer 0 :adjustable t)))
             (scan (i)
               (when (< i n)
                 (let* ((elt (aref a i))
                        (bin (get-bin-for-elt elt as)))
                   (vector-push-extend i bin)
                   (setf (gethash elt as) bin
                         (aref deltas i) (- sum elt))
                   (scan (1+ i)))) )
             (seek (i)
               (if (= i n)
                   nil
                   (let* ((delta (aref deltas i))
                          (hit (gethash delta as)))
                     (if (and hit (notevery (partial #'= i) hit))
                         (list (aref a i) delta)
                         (seek (1+ i)))) )))
      (scan 0)
      (seek 0))))

(deftest test-two-sum ()
  (check
   (null (two-sum #(0 0 0 0 0 0)))
   (null (two-sum #(1 2 3 4 5 4 3 2 1)))
   (null (two-sum #(9 7 6 5)))
   (equal '(2 8) (two-sum #(2 8 1 4 7)))
   (equal '(8 2) (two-sum #(8 2 1 4 7)))
   (equal '(3 7) (two-sum #(1 2 3 1 2 7)))
   (equal '(3 7) (two-sum #(1 2 3 5 4 1 2 6 5 7)))
   (equal '(5 5) (two-sum #(1 2 3 5 4 1 2 6 5)))
   (equal '(4 6) (two-sum #(1 2 3 5 4 1 2 6)))
   (equal '(5 5) (two-sum #(1 2 5 4 3 5 1 2 6 7)))
   (equal '(4 6) (two-sum #(1 2 5 4 3 1 2 6 7)))
   (equal '(7 3) (two-sum #(7 1 2 5 4 3 1 2 6 7)))) )

;;;
;;;    4.
;;;    
(defun contains (s &optional (target #\X))
  (loop for ch across s
        if (char= ch target) do (return t)
        finally (return nil)))

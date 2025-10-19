;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Fri Jul  4 16:26:46 2025
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

(defpackage :ch04 (:use :common-lisp :core :test))

(in-package :ch04)

;;;
;;;    3.
;;;    
(defun greatest-product (a)
  (assert (>= (length a) 2) () "Get serious!")
  (loop with max = (* (aref a 0) (aref a 1))
        for i from 0 below (length a)
        do (loop for j from (1+ i) below (length a)
                 for prod = (* (aref a i) (aref a j))
                 when (> prod max)
                 do (setf max prod))
        finally (return max)))

(defun greatest-product (a)
  (assert (>= (length a) 2) () "Get serious!")
  (loop with max = (* (aref a 0) (aref a 1))
        for i from 0 below (length a)
        for ival across a
        do (loop for j from (1+ i) below (length a)
                 for jval across (subseq a (1+ i))
                 for prod = (* ival jval)
                 when (> prod max)
                 do (setf max prod))
        finally (return max)))

(defun greatest-product (a)
  (reduce #'max
          (mapcar (partial #'apply #'*)
                  (loop for ival across a
                        nconc (loop for jval across a 
                                    unless (= ival jval) 
                                    collect (list ival jval)))) ))

;; (loop for ival across a
;;       nconc (loop for jval across a 
;;                   unless (= ival jval) 
;;                   collect (list ival jval))))

;;;
;;;    Removes diagonal, but "duplicates" remain:
;;;    (greatest-product #(2 4 6 8))
;;;    ((2 4) (2 6) (2 8) (4 2) (4 6) (4 8) (6 2) (6 4) (6 8) (8 2) (8 4) (8 6))
;;;    

(defun greatest-product (a)
  (reduce #'max
          (mapcar (partial #'apply #'*)
                  (loop for i from 0 below (length a)
                        nconc (loop for j from (1+ i) below (length a)
                                    collect (list (aref a i) (aref a j)))) )))

;;;
;;;   Why not get right to the point?
;;;   
(defun greatest-product (a)
  (reduce #'max
          (loop for i from 0 below (length a)
                nconc (loop for j from (1+ i) below (length a)
                            collect (* (aref a i) (aref a j)))) ))

(defun greatest-product (a)
  (symbol-macrolet ((aᵢ (aref a i))
                    (aⱼ (aref a j)))
    (reduce #'max
            (loop for i from 0 below (length a)
                  nconc (loop for j from (1+ i) below (length a)
                              collect (* aᵢ aⱼ)))) ))

(defun greatest-product (a)
  (reduce #'max
          (loop for i from 0 below (length a)
                for aᵢ = (aref a i)
                nconc (loop for j from (1+ i) below (length a)
                            for aⱼ = (aref a j)
                            collect (* aᵢ aⱼ)))) )

;;;
;;;    4.
;;;
(defun greatest-number (a)
  (loop for outer across a
        for outer-is-greatest = t
        do (loop for inner across a
                 if (> inner outer)
                 do (setf outer-is-greatest nil))
        if outer-is-greatest
        do (return outer)))

(defun greatest-number (a)
  (loop with max = (aref a 0)
        for i from 1 below (length a)
        if (> (aref a i) max)
        do (setf max (aref a i))
        finally (return max)))

(defun greatest-number (a)
  (loop with max = (aref a 0)
        for i from 1 below (length a)
        do (setf max (max max (aref a i)))
        finally (return max)))

(defun greatest-number (a)
  (reduce #'max a))

(defun greatest-number (a)
  (apply #'max (coerce a 'list)))

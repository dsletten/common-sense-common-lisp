#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               ch01.lisp
;;;;
;;;;   Started:            Fri Mar 14 23:12:45 2025
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
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/test")

(defpackage :ch01
  (:use :common-lisp :core :test)
  (:shadow :delete))

(in-package :ch01)

(defvar *shopping-list* (make-array 5
                                    :fill-pointer t
                                    :initial-contents (list "apples" "bananas" "cucumbers" "dates" "elderberries")))

(defun insert (a i obj)
  (assert (<= 0 i (length a)) () "Invalid index: ~A" i)
  (flet ((shift-up ()
           (setf (subseq a (1+ i)) (subseq a i))))
    (vector-push-extend nil a)
    (shift-up)
    (setf (aref a i) obj)))

(defun delete (a i)
  (assert (<= 0 i (1- (length a))) () "Invalid index: ~A" i)
  (flet ((shift-down ()
           (setf (subseq a i) (subseq a (1+ i)))) )
    (prog1 (aref a i)
      (shift-down)
      (vector-pop a))))

(format t "Shopping list: ~A~%" *shopping-list*)

(format t "Element at index 2: ~A~%" (aref *shopping-list* 2))

(format t "Index of element \"dates\": ~D~%" (position "dates" *shopping-list* :test #'string=))

(format t "Add \"figs\" to list.~%")
(insert *shopping-list* 2 "figs")
(format t "Shopping list: ~A~%" *shopping-list*)

(format t "Remove element at index 3: ~A.~%" (delete *shopping-list* 3))
(format t "Shopping list: ~A~%" *shopping-list*)

(format t "Add more \"figs\" to list.~%")
(insert *shopping-list* 0 "figs")
(format t "Shopping list: ~A~%" *shopping-list*)

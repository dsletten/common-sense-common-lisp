#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               ch01a.lisp
;;;;
;;;;   Started:            Wed Mar 19 19:34:04 2025
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
(load "/home/slytobias/lisp/books/CommonSense/ch01/array-set.lisp")

(defpackage :ch01a (:use :common-lisp :core :test))

(in-package :ch01a)

(defvar *shopping-list* (as:make-array-set '("apples" "bananas" "cucumbers" "dates" "elderberries")))

(format t "Shopping list: ~A~%" *shopping-list*)

(format t "Element at index 2: ~A~%" (as:read *shopping-list* 2))

(format t "Index of element \"dates\": ~D~%" (as:search *shopping-list* "dates"))

(format t "Add \"figs\" to list.~%")
(as:insert *shopping-list* 2 "figs")
(format t "Shopping list: ~A~%" *shopping-list*)

(format t "Remove element at index 3: ~A.~%" (as:delete *shopping-list* 3))
(format t "Shopping list: ~A~%" *shopping-list*)

(format t "Add more \"figs\" to list.~%")
(as:insert *shopping-list* 0 "figs")
(format t "Shopping list: ~A~%" *shopping-list*)


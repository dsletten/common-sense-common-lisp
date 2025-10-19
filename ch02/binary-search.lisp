;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               binary-search.lisp
;;;;
;;;;   Started:            Sun Apr  6 00:48:22 2025
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
;;;;   Notes: 见 ~/lisp/books/Kubica/ch02/ch02.lisp
;;;;
;;;;   The default TEST possible in Common Lisp is not as cool as it seems. It
;;;;   is biased towards arrays of numbers in ascending order. Any other type
;;;;   requires an explicit argument as does any array sorted in descending order.
;;;;
;;;;   This default is not possible in Java. There is no general-purpose Comparator<T>
;;;;   for arbitrary type T. So putting this in perspective, what is not a major
;;;;   convenience in Lisp is not a major inconvenience if Java lacks it.
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :binary-search (:use :common-lisp :core :test) (:shadow :search))

(in-package :binary-search)

(defun binary-search (a target &key (key #'identity) (test #'<))
  (labels ((search (low high)
             (if (< high low)
                 (- (1+ low))
                 (let* ((mid (truncate (+ low high) 2))
                        (current (funcall key (aref a mid))))
                   (cond ((funcall test current target) (search (1+ mid) high))
                         ((funcall test target current) (search low (1- mid)))
                         (t mid)))) ))
    (search 0 (1- (length a)))) )

(deftest test-binary-search ()
  (check
   (let ((a #(-5 -1 0 3 9 11 15 17 30 35 51 54)))
     (check
      (= 0 (binary-search a -5))
      (= 3 (binary-search a 3))
      (= 4 (binary-search a 9))
      (= 8 (binary-search a 30))
      (= 11 (binary-search a 54))
      (= -1 (binary-search a -8))
      (= -7 (binary-search a 12))
      (= -13 (binary-search a 60))))
   (let ((a (reverse #(-5 -1 0 3 9 11 15 17 30 35 51 54))))
     (check
      (= 0 (binary-search a 54 :test #'>))
      (= 3 (binary-search a 30 :test #'>))
      (= 4 (binary-search a 17 :test #'>))
      (= 8 (binary-search a 3 :test #'>))
      (= 11 (binary-search a -5 :test #'>))
      (= -13 (binary-search a -8 :test #'>))
      (= -7 (binary-search a 12 :test #'>))
      (= -1 (binary-search a 60 :test #'>))))
   (let ((a #("Clojure" "java" "JavaScript" "LISP" "Prolog" "ruby")))
     (check
      (= 0 (binary-search a "clojure" :test #'string-lessp))
      (= 1 (binary-search a "Java" :test #'string-lessp))
      (= 2 (binary-search a "JAVASCRIPT" :test #'string-lessp))
      (= 3 (binary-search a "Lisp" :test #'string-lessp))
      (= 4 (binary-search a "prolog" :test #'string-lessp))
      (= 5 (binary-search a "Ruby" :test #'string-lessp))
      (= -5 (binary-search a "oz" :test #'string-lessp))
      (= -1 (binary-search a "C#" :test #'string-lessp))))
   (check ; Duplicate elements
    (= 2 (binary-search #(1 2 3 3d0 4) 3))
    (= 2 (binary-search #(1 2 3 3d0) 3))
    (= 1 (binary-search #(2 3 3d0 4) 3))
    (= 1 (binary-search #(3 3d0 4) 3))
    (= 4 (binary-search #(0 1 2 3 3d0 4) 3)))
   (let ((a #((a . 1) (b . 3) (b . 2) (c . 1) (c . 5))))
     (flet ((symbol< (a b)
              (string< (symbol-name a) (symbol-name b))))
       (check
        (= 2 (binary-search a 'b :test #'symbol< :key #'car))
        (= 3 (binary-search a 'c :test #'symbol< :key #'car))
        (= 0 (binary-search a 'a :test #'symbol< :key #'car)))) )))

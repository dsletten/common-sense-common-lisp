;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               duplicates.lisp
;;;;
;;;;   Started:            Sun May 11 22:13:49 2025
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

(defpackage :duplicates
  (:use :common-lisp :core :test)
  (:shadow :remove-duplicates))

(in-package :duplicates)

;;;
;;;    Only works for sequences of numbers. Equality test hard-wired: =
;;;    No need to test for (= i j) as book does!
;;;    
(defun has-duplicates-quadratic (a)
  (loop for i from 0 below (length a)
        do (loop for j from (1+ i) below (length a) ; 0 iterations of J on final pass for I
                 when (= (aref a i) (aref a j))
                 do (return-from has-duplicates-quadratic t))
        finally (return nil)))


;; (defun has-duplicates-quadratic* (a)
;;   (labels ((outer (i)
;;              (cond ((= i (length a)) nil)
;;                    (t (or (inner (aref a i) (1+ i))
;;                           (outer (1+ i)))) ))
;;            (inner (elt j)
;;              (cond ((= j (length a)) nil)
;;                    ((= elt (aref a j)) t)
;;                    (t (inner elt (1+ j)))) ))
;;     (outer 0)))

(defun has-duplicates-quadratic* (a)
  (labels ((outer (i)
             (if (= i (length a))
                 nil
                 (or (inner (aref a i) (1+ i))
                     (outer (1+ i)))) )
           (inner (elt j)
             (if (= j (length a))
                 nil
                 (or (= elt (aref a j))
                     (inner elt (1+ j)))) ))
    (outer 0)))

(defun has-duplicates (a)
  (loop with seen = (make-hash-table :test #'eql)
        for elt across a
        do (if (gethash elt seen)
               (return t)
               (setf (gethash elt seen) t))
        finally (return nil)))

;; (defun has-duplicates (a)
;;   (let ((seen (make-hash-table :test #'eql)))
;;     (labels ((search-array (i)
;;                (cond ((= i (length a)) nil)
;;                      ((gethash (aref a i) seen) t)
;;                      (t (setf (gethash (aref a i) seen) t)
;;                         (search-array (1+ i)))) ))
;;       (search-array 0))))

(defun has-duplicates* (a)
  (let ((seen (make-hash-table :test #'eql)))
    (labels ((search-array (i)
               (if (= i (length a))
                   nil
                   (or (gethash (aref a i) seen)
                       (and (setf (gethash (aref a i) seen) t)
                            (search-array (1+ i)))) )))
      (search-array 0))))

(deftest test-has-duplicates (f)
  (check
   (funcall f #(1 5 3 9 1 4))
   (not (funcall f #(1 4 5 2 9)))) )

;;;
;;;    This differs from CL:REMOVE-DUPLICATES
;;;    • This is less flexible. No :FROM-END, limitations on :TEST
;;;    • This keeps earlier elements and drops later:
;;;    (REMOVE-DUPLICATES '(0 1 0 2 0 3 0)) => (0 1 2 3)
;;;    (CL:REMOVE-DUPLICATES '(0 1 0 2 0 3 0)) => (1 2 3 0)
;;;    
(defun remove-duplicates (a)
  (let ((cache (make-hash-table :test #'eql)))
    (labels ((seen (elt)
               (cond ((gethash elt cache) t)
                     (t (setf (gethash elt cache) t)
                        nil))))
      (remove-if #'seen a))))

(defun has-duplicates-list-quadratic (l)
  (loop for i on l
        do (loop for j on (rest i) ; 0 iterations on final pass for i
                 when (= (first i) (first j))
                 do (return-from has-duplicates-list-quadratic t))
        finally (return nil)))

;; (defun has-duplicates-list-quadratic* (l)
;;   (labels ((outer (i)
;;              (cond ((null i) nil)
;;                    (t (or (inner (first i) (rest i))
;;                           (outer (rest i)))) ))
;;            (inner (elt j)
;;              (cond ((null j) nil)
;;                    ((= elt (first j)) t)
;;                    (t (inner elt (rest j)))) ))
;;     (outer l)))

(defun has-duplicates-list-quadratic* (l)
  (labels ((outer (i)
             (if (null i)
                 nil
                 (or (inner (first i) (rest i))
                     (outer (rest i)))) )
           (inner (elt j)
             (if (null j)
                 nil
                 (or (= elt (first j))
                     (inner elt (rest j)))) ))
    (outer l)))

(defun has-duplicates-list (l)
  (loop with seen = (make-hash-table :test #'eql)
        for elt in l
        do (if (gethash elt seen)
               (return t)
               (setf (gethash elt seen) t))
        finally (return nil)))

;; (defun has-duplicates-list* (l)
;;   (let ((seen (make-hash-table :test #'eql)))
;;     (labels ((search-list (l)
;;                (if (null l)
;;                    nil
;;                    (destructuring-bind (first . rest) l
;;                      (cond ((gethash first seen) t)
;;                            (t (setf (gethash first seen) t)
;;                               (search-list rest)))) )))
;;       (search-list l))))

(defun has-duplicates-list* (l)
  (let ((seen (make-hash-table :test #'eql)))
    (labels ((search-list (l)
               (if (null l)
                   nil
                   (destructuring-bind (first . rest) l
                     (or (gethash first seen)
                         (and (setf (gethash first seen) t) ; Sketchy? Relies on value of SETF
                              (search-list rest)))) )))
      (search-list l))))

(deftest test-has-duplicates-list (f)
  (check
   (funcall f '(1 5 3 9 1 4))
   (not (funcall f '(1 4 5 2 9)))) )

;;;
;;;    For short arrays, the naive version can win...
;;;
;; * (time (loop repeat 10000 do (has-duplicates #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.003 seconds of real time
;;   0.002968 seconds of total run time (0.002968 user, 0.000000 system)
;;   100.00% CPU
;;   9,451,729 processor cycles
;;   3,366,992 bytes consed
  
;; NIL
;; * (time (loop repeat 10000 do (has-duplicates-quadratic #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.002 seconds of real time
;;   0.001660 seconds of total run time (0.001660 user, 0.000000 system)
;;   100.00% CPU
;;   5,335,116 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (time (loop repeat 10000 do (has-duplicates-quadratic* #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.001416 seconds of total run time (0.001416 user, 0.000000 system)
;;   100.00% CPU
;;   4,508,074 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (time (loop repeat 100000 do (has-duplicates #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.024 seconds of real time
;;   0.023464 seconds of total run time (0.023464 user, 0.000000 system)
;;   [ Real times consist of 0.003 seconds GC time, and 0.021 seconds non-GC time. ]
;;   [ Run times consist of 0.003 seconds GC time, and 0.021 seconds non-GC time. ]
;;   95.83% CPU
;;   74,788,115 processor cycles
;;   33,539,952 bytes consed
  
;; NIL
;; * (time (loop repeat 100000 do (has-duplicates-quadratic #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.011 seconds of real time
;;   0.010845 seconds of total run time (0.010845 user, 0.000000 system)
;;   100.00% CPU
;;   34,556,785 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (time (loop repeat 1000000 do (has-duplicates-quadratic #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.027 seconds of real time
;;   0.027010 seconds of total run time (0.027010 user, 0.000000 system)
;;   100.00% CPU
;;   86,080,126 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (time (loop repeat 1000000 do (has-duplicates #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.092 seconds of real time
;;   0.091690 seconds of total run time (0.089696 user, 0.001994 system)
;;   [ Real times consist of 0.008 seconds GC time, and 0.084 seconds non-GC time. ]
;;   [ Run times consist of 0.008 seconds GC time, and 0.084 seconds non-GC time. ]
;;   100.00% CPU
;;   292,182,845 processor cycles
;;   335,957,760 bytes consed
  
;; NIL
;; * (time (loop repeat 1000000 do (has-duplicates-quadratic #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.029 seconds of real time
;;   0.029317 seconds of total run time (0.029316 user, 0.000001 system)
;;   100.00% CPU
;;   93,612,184 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (time (loop repeat 1000000 do (has-duplicates-quadratic* #(1 5 3 9 1 4))))
;; Evaluation took:
;;   0.037 seconds of real time
;;   0.037305 seconds of total run time (0.037304 user, 0.000001 system)
;;   100.00% CPU
;;   118,917,763 processor cycles
;;   0 bytes consed
  
;; NIL

;;;
;;;    For large arrays, the naive version gets crushed as expected!
;;;    
;; * (let ((v (coerce (loop for i from 1 to 10000 collect i) 'vector))) (time (has-duplicates v)))
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.000946 seconds of total run time (0.000946 user, 0.000000 system)
;;   100.00% CPU
;;   3,007,588 processor cycles
;;   794,560 bytes consed
  
;; NIL
;; * (let ((v (coerce (loop for i from 1 to 10000 collect i) 'vector))) (time (has-duplicates-quadratic v)))
;; Evaluation took:
;;   0.203 seconds of real time
;;   0.202980 seconds of total run time (0.202980 user, 0.000000 system)
;;   100.00% CPU
;;   647,093,165 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((v (coerce (loop for i from 1 to 10000 collect i) 'vector))) (time (has-duplicates-quadratic* v)))
;; Evaluation took:
;;   0.294 seconds of real time
;;   0.293911 seconds of total run time (0.293911 user, 0.000000 system)
;;   100.00% CPU
;;   936,805,279 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((v (coerce (loop for i from 1 to 100000 collect i) 'vector))) (time (has-duplicates v)))
;; Evaluation took:
;;   0.003 seconds of real time
;;   0.002128 seconds of total run time (0.002128 user, 0.000000 system)
;;   66.67% CPU
;;   6,781,256 processor cycles
;;   8,756,656 bytes consed
  
;; NIL
;; * (let ((v (coerce (loop for i from 1 to 100000 collect i) 'vector))) (time (has-duplicates-quadratic v)))
;; Evaluation took:
;;   19.809 seconds of real time
;;   19.808385 seconds of total run time (19.808385 user, 0.000000 system)
;;   99.99% CPU
;;   63,135,120,929 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((v (coerce (loop for i from 1 to 100000 collect i) 'vector))) (time (has-duplicates-quadratic* v)))
;; Evaluation took:
;;   28.542 seconds of real time
;;   28.539941 seconds of total run time (28.539941 user, 0.000000 system)
;;   99.99% CPU
;;   90,964,658,011 processor cycles
;;   0 bytes consed
  
;; NIL

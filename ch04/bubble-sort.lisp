;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               bubble-sort.lisp
;;;;
;;;;   Started:            Sun Apr 20 23:33:47 2025
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
(load "/home/slytobias/lisp/books/Concise/concise.lisp")

(defpackage :bubble-sort (:use :common-lisp :core :test))

(in-package :bubble-sort)

;; (defun bubble-sort-1 (a test &key (key #'identity))
;;   (loop for i from (1- (length a)) downto 1 ; Don't need to "bubble" 0th elt
;;         for swapped = nil
;;         do (loop for j from 1 upto i
;;                  do (format t "i: ~D j: ~D ~A~%" i j a)
;;                  if (funcall test (funcall key (aref a j)) (funcall key (aref a (1- j))))
;;                  do (rotatef (aref a j) (aref a (1- j)))
;;                     (setf swapped t))
;;         unless swapped do (progn (format t "Done: ~D~%" i) (return a))
;;         finally (return a)))

(defun bubble-sort-2 (a test &key (key #'identity))
  (loop for i from (1- (length a)) downto 1 ; Don't need to "bubble" 0th elt
        for swapped = nil
        do (loop for j from 0 below i
;                 do (format t "i: ~D j: ~D ~A~%" i j a)
                 if (funcall test (funcall key (aref a (1+ j))) (funcall key (aref a j)))
                 do (rotatef (aref a (1+ j)) (aref a j))
                    (setf swapped t))
        unless swapped 
        do (format t "Done: ~D~%" i) 
           (return a)
        finally (return a)))

(defun should-swap-p (a b test &optional key)
  (if (null key)
      (funcall test a b)
      (funcall test (funcall key a) (funcall key b))))

(defun bubble-sort-2* (a test &key key)
  (loop for i from (1- (length a)) downto 1 ; Don't need to "bubble" 0th elt
        for swapped = nil
        do (loop for j from 0 below i
;                 do (format t "i: ~D j: ~D ~A~%" i j a)
                 if (should-swap-p (aref a (1+ j)) (aref a j) test key)
                 do (rotatef (aref a (1+ j)) (aref a j))
                    (setf swapped t))
        unless swapped 
        do (format t "Done: ~D~%" i) 
           (return a)
        finally (return a)))

;;;
;;;    Same as 2 but DOTIMES vs. LOOP (Also helper function SHOULD-SWAP-P)
;;;    
(defun bubble-sort-2a (a test &key key)
  (loop for i from (1- (length a)) downto 1 ; Don't need to "bubble" 0th elt
        for swapped = nil
        do (dotimes (j i)
;                 do (format t "i: ~D j: ~D ~A~%" i j a)
             (when (should-swap-p (aref a (1+ j)) (aref a j) test key)
               (rotatef (aref a (1+ j)) (aref a j))
               (setf swapped t)))
        unless swapped 
        do (format t "Done: ~D~%" i) 
           (return a)
        finally (return a)))

;;;
;;;    Same as 2a but SVREF vs. AREF
;;;    
(defun bubble-sort-2b (a test &key key)
  (loop for i from (1- (length a)) downto 1 ; Don't need to "bubble" 0th elt
        for swapped = nil
        do (dotimes (j i)
;                 do (format t "i: ~D j: ~D ~A~%" i j a)
             (when (should-swap-p (svref a (1+ j)) (svref a j) test key)
               (rotatef (svref a (1+ j)) (svref a j))
               (setf swapped t)))
        unless swapped 
        do (format t "Done: ~D~%" i) 
           (return a)
        finally (return a)))

;;;
;;;    Same as 2 but helper functions/SYMBOL-MACROLET abstractions.
;;;    Improved readability?
;;;    
(defun bubble-sort-3 (a test &key key)
  (symbol-macrolet ((low (aref a j))
                    (high (aref a (1+ j))))
    (loop for i from (1- (length a)) downto 1 ; Don't need to "bubble" 0th elt
          for swapped = nil
          do (loop for j from 0 below i
;;                   do (format t "i: ~D j: ~D ~A~%" i j a)
                   if (should-swap-p high low test key)
                   do (rotatef high low)
                      (setf swapped t))
          unless swapped 
          do (format t "Done: ~D~%" i) 
             (return a)
          finally (return a))))

;;;
;;;    Recursive
;;;    
(defun bubble-sort-4 (a test &key key)
  (symbol-macrolet ((low (aref a j))
                    (high (aref a (1+ j))))
    (labels ((swap-free-p (result) result) ; Kludge to improve readability of BUBBLE-UP return value.
             (bubble-down (i)
               (cond ((zerop i) a)
                     ((swap-free-p (bubble-up i 0 t)) a)
                     (t (bubble-down (1- i)))) )
             (bubble-up (i j swap-free) ; Return T to indicate no swap occurred.
;               (format t "i: ~D j: ~D ~A~%" i j a)
               (cond ((= i j) swap-free)
                     ((should-swap-p high low test key)
                      (rotatef high low)
                      (bubble-up i (1+ j) nil))
                     (t (bubble-up i (1+ j) swap-free)))) )
      (if (emptyp a)
          a
          (bubble-down (1- (length a)))) )))

;;;
;;;    Bubble sort on lists
;;;
;;;    I. Terrible to traverse list backwards (nthcdr (1- (length l)) l)
;;;                                         I           
;;;                                         |           
;;;                                         v           
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;                                         |           
;;;                                         v           
;;;                                         3           
;;;                                                     
;;;     J                                               
;;;     |                                               
;;;     v                                               
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;     |        |        |        |                    
;;;     v        v        v        v                    
;;;     5        4        6        1                    
;;;                                                     
;;;                                I                    
;;;                                |                    
;;;                                v                    
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;                                |        |           
;;;                                v        v           
;;;                                3        6           
;;;                                                     
;;;     J                                               
;;;     |                                               
;;;     v                                               
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;     |        |        |                             
;;;     v        v        v                             
;;;     4        5        1
;;;                                                     
;;;                       I                             
;;;                       |                             
;;;                       v                             
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;                       |        |        |           
;;;                       v        v        v           
;;;                       3        5        6
;;;                                                     
;;;     J                                               
;;;     |                                               
;;;     v                                               
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;     |        |                                      
;;;     v        v                                      
;;;     4        1                                  
;;;                                                     
;;;              I                                      
;;;              |                                      
;;;              v                                      
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;              |        |        |        |           
;;;              v        v        v        v           
;;;              3        4        5        6
;;;                                                     
;;;     J                                               
;;;     |                                               
;;;     v                                               
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL 
;;;     |                                               
;;;     v                                               
;;;     1
;;;                                                     
(defun bubble-sort-list-1 (l test &key key)
  (loop for i from (1- (length l)) downto 1 ; LENGTH is an additional list traversal!
        for tail = (nthcdr i l)
        for swapped = nil
        do (loop for j on l
                 until (eq j tail)
;                 do (format t "tail: ~D j: ~D ~A~%" tail j l)
                 if (should-swap-p (second j) (first j) test key)
                 do (rotatef (second j) (first j))
                    (setf swapped t))
        unless swapped do (progn (format t "Done: ~D~%" i) (return l))
        finally (return l)))

;;;
;;;    II. Use call stack to hold references to tails. Not tail recursive. May overflow stack:
;;;    (bubble-sort-list-2 (coerce (shuffle (coerce (loop for i from 1 to 100000 collect i) 'vector)) 'list) #'<)
;;;    
;;;    These are all slow! But they complete:
;;;    (bubble-sort-list-3 (coerce (shuffle (coerce (loop for i from 1 to 100000 collect i) 'vector)) 'list) #'<)
;;;    (bubble-sort-list-4 (coerce (shuffle (coerce (loop for i from 1 to 100000 collect i) 'vector)) 'list) #'<)
;;;    (bubble-sort-list-5 (coerce (shuffle (coerce (loop for i from 1 to 100000 collect i) 'vector)) 'list) #'<)
;;;
;;;                                                    I                                      
;;;                                                    |                                     
;;;                                                    v                                     
;;;                                  +---->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  |                 |        |        |        |          
;;;                                  |                 v        v        v        v          
;;;                                  |                 4        6        1        3          
;;;                                  |                                                       
;;;                                  |                          I                            
;;;                                  |                          |                            
;;;                                  |                          v                            
;;;                                  |+--->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  ||                         |        |        |          
;;;                                  ||                         v        v        v          
;;;                                  ||                         6        1        3
;;;                                  ||                                                      
;;;                                  ||                                  I                   
;;;                                  ||                                  |                   
;;;                                  ||                                  v                   
;;;                                  ||+-->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  |||                                 |        |          
;;;                                  |||                                 v        v          
;;;                                  |||                                 1        3
;;;                                  |||                                                     
;;;                                  |||                                          I          
;;;                                  |||                                          |          
;;;                                  |||                                          v          
;;;                                  |||+->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  ||||                                         |          
;;;                                  ||||                                         v          
;;;                                  ||||                                         3          
;;;                                  ||||                                                    
;;;                                  ||||     J                                              
;;;                                  ||||     |                                              
;;;                                  ||||     v                                              
;;;                                  |||+->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  |||      |        |        |        |                   
;;;                                  |||      v        v        v        v                   
;;;                                  |||      4        5        1        3          
;;;                                  |||                                                     
;;;                                  |||      J                                              
;;;                                  |||      |                                              
;;;                                  |||      v                                              
;;;                                  ||+-->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  ||       |        |        |                            
;;;                                  ||       v        v        v                            
;;;                                  ||       4        1        3
;;;                                  ||                                                      
;;;                                  ||       J                                              
;;;                                  ||       |                                              
;;;                                  ||       v                                              
;;;                                  |+--->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;                                  |        |        |                                     
;;;                                  |        v        v                                     
;;;                                  |        1        3                            
;;;                                  |                                                       
;;;                                  |        J                                              
;;;                                  |        |                                              
;;;                                  |        v                                              
;;;                                  +---->  [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL       <-- Skipped
;;;                                           |                                              
;;;                                           v                                              
;;;                                           1                                              
(defun bubble-sort-list-2 (l test &key key)
  (labels ((swap-free-p (result) result)
           (bubble-down (tail)
;             (format t "tail: ~A~%" tail)
             (when tail
               (or (bubble-down (rest tail))
                   (swap-free-p (bubble-up tail l t)))) )
           (bubble-up (tail j swap-free) ; Return T to indicate no swap occurred.
;             (format t "tail: ~A j: ~A ~A~%" tail j l)
             (cond ((eq j tail) swap-free)
                   ((should-swap-p (second j) (first j) test key)
                    (rotatef (second j) (first j))
                    (bubble-up tail (rest j) nil))
                   (t (bubble-up tail (rest j) swap-free)))) )
    (if (emptyp l)
        l
        (bubble-down (rest l)))
    l))

;;;
;;;    III. Use explicit stack (mapl (partial #'push stack) l)
;;;
;;;   +--------------------------------------------------+
;;;   |                                      I           |
;;;   |                                      |           |
;;;   |                                      v           |
;;;   | [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL |
;;;   |                                      |           |
;;;   |                                      v           |
;;;   |                                      3           |
;;;   +--------------------------------------------------+
;;;   |                             I                    |
;;;   |                             |                    |
;;;   |                             v                    |
;;;   | [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL |
;;;   |                             |        |           |
;;;   |                             v        v           |
;;;   |                             3        6           |
;;;   +--------------------------------------------------+
;;;   |                    I                             |
;;;   |                    |                             |
;;;   |                    v                             |
;;;   | [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL |
;;;   |                    |        |        |           |
;;;   |                    v        v        v           |
;;;   |                    3        5        6           |
;;;   +--------------------------------------------------+
;;;   |           I                                      |
;;;   |           |                                      |
;;;   |           v                                      |
;;;   | [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL |
;;;   |           |        |        |        |           |
;;;   |           v        v        v        v           |
;;;   |           3        4        5        6           |
;;;   +--------------------------------------------------+
;;;   
(defun bubble-sort-list-3 (l test &key key)
  (if (emptyp l)
      l
      (let ((stack (make-instance 'containers:linked-stack)))
        (mapl (partial #'containers:push stack) (rest l))
        (loop until (containers:emptyp stack)
              for tail = (containers:pop stack)
              for swapped = nil
              do (loop for j on l
                       until (eq j tail)
;                       do (format t "tail: ~A j: ~A ~A~%" tail j l)
                       if (should-swap-p (second j) (first j) test key)
                       do (rotatef (second j) (first j))
                          (setf swapped t))
              unless swapped do (progn (format t "Done: ~A~%" tail) (return l))
              finally (return l)))) )

;;;
;;;    Pseudo-FP: Persistent stack, but L is modified in place.
;;;    
(defun bubble-sort-list-4 (l test &key key)
  (labels ((swap-free-p (result) result)
           (capture-tails (tail stack)
             (if (endp tail)
                 stack
                 (capture-tails (rest tail) (containers:push stack tail))))
           (bubble-down (stack)
             (cond ((containers:emptyp stack) l)
                   ((swap-free-p (bubble-up (containers:peek stack) l t)) l)
                   (t (bubble-down (containers:pop stack)))) )
           (bubble-up (tail j swap-free) ; Return T to indicate no swap occurred.
;             (format t "tail: ~A j: ~A ~A~%" tail j l)
             (cond ((eq j tail) swap-free)
                   ((should-swap-p (second j) (first j) test key)
                    (rotatef (second j) (first j))
                    (bubble-up tail (rest j) nil))
                   (t (bubble-up tail (rest j) swap-free)))) )
    (if (emptyp l)
        l
        (bubble-down (capture-tails (rest l) (make-instance 'containers:persistent-linked-stack)))) ))

;;;
;;;    Same as 4 but uses REDUCE to build stack.
;;;    
(defun bubble-sort-list-5 (l test &key key)
  (labels ((swap-free-p (result) result)
           (bubble-down (stack)
             (cond ((containers:emptyp stack) l)
                   ((swap-free-p (bubble-up (containers:peek stack) l t)) l)
                   (t (bubble-down (containers:pop stack)))) )
           (bubble-up (tail j swap-free) ; Return T to indicate no swap occurred.
;             (format t "tail: ~A j: ~A ~A~%" tail j l)
             (cond ((eq j tail) swap-free)
                   ((should-swap-p (second j) (first j) test key)
                    (rotatef (second j) (first j))
                    (bubble-up tail (rest j) nil))
                   (t (bubble-up tail (rest j) swap-free)))) )
    (if (emptyp l)
        l
        (bubble-down (reduce #'containers:push (maplist #'identity (rest l))
                             :initial-value (make-instance 'containers:persistent-linked-stack)))) ))



;;;    IV. Tie in to Fox's correlation of recursion/stacks





;; Java books
;; Nutshell/ch01/SortNumbers/SortNumbers.java
;; Concise/javaDataStructures/sortingSearching/Sorts.java
;; FundamentalsOfOOP/bookcode/foundations/ch18-Sorting/SortLab/SortMethods.java
;; LearningJava/examples/ch22/magicbeans/src/magicbeans/sunw/demo/molecule/XYZChemModel.java
;; CodingProblems/Java-Coding-Problems/Chapter05/P99_SortArray/src/modern/challenge/ArraySorts.java

;; JustJava/CDROM/goodies/Acme/Utils.java
;; JustJava/CDROM/goodies/vsort/BidirBubbleSortAlgorithm.java
;; JustJava/CDROM/goodies/vsort/BidirectionalBubbleSortAlgorithm.java
;; JustJava/CDROM/goodies/vsort/BubbleSortAlgorithm.java
;; JustJava/CDROM/goodies/vsort/SortItem.java
;; Projects/JavaProjects_Code/Chapter03/genericsort/bubble/src/main/java/packt/java189fundamentals/ch03/main/bubble/generic/BubbleSort.java
;; Projects/JavaProjects_Code/Chapter03/genericsort/bubble/src/main/java/packt/java189fundamentals/ch03/main/bubble/BubbleSort.java
(defun random-ordered-list ()
  (let* ((random-state (make-random-state t))
         (low (random 200 random-state))
         (high (+ low (random 100 random-state))))
    (loop for i from low upto high collect i)))

(defun random-ordered-array ()
  (coerce (random-ordered-list) 'vector))

;(test-bubble-sort #'bubble-sort-list-5 'list)

(deftest test-bubble-sort (bubble-sort type)
  (check
   (eqls (apply type '())
         (funcall bubble-sort (apply type '()) #'<))
   (eqls (apply type '(10))
         (funcall bubble-sort (apply type '(10)) #'<))
   (eqls (apply type '(10 20))
         (funcall bubble-sort (apply type '(10 20)) #'<))
   (eqls (apply type '(10 20))
         (funcall bubble-sort (apply type '(20 10)) #'<))
   (eqls (apply type '(1 1d0 2 3d0 3))
         (funcall bubble-sort (apply type '(3d0 1 2 3 1d0)) #'<)) ; Stable
   (eqls (apply type '(3d0 3 2 1 1d0))
         (funcall bubble-sort (apply type '(3d0 1 2 3 1d0)) #'>)) ; Stable
   (eqls (apply type '(2 2 4 4 6 6 8 8))
         (funcall bubble-sort (apply type '(2 4 6 8 2 4 6 8)) #'<))
   (eqls (apply type '(8 8 6 6 4 4 2 2))
         (funcall bubble-sort (apply type '(2 4 6 8 2 4 6 8)) #'>))
   (eqls (apply type '(1 2 3 4 5))
         (funcall bubble-sort (apply type '(1 2 3 4 5)) #'<))
   (eqls (apply type '(1 2 3 4 5))
         (funcall bubble-sort (apply type '(5 4 3 2 1)) #'<))
   (eqls (apply type '(5 4 3 2 1))
         (funcall bubble-sort (apply type '(1 2 3 4 5)) #'>))
   (eqls (apply type '(5 4 3 2 1))
         (funcall bubble-sort (apply type '(5 4 3 2 1)) #'>))
   (eqls (apply type '(1/5 1/4 1/3 1/2 1))
         (funcall bubble-sort (apply type '(1 1/3 1/5 1/2 1/4)) #'<))
   (eqls (apply type '(1 1/2 1/3 1/4 1/5))
         (funcall bubble-sort (apply type '(1 1/3 1/5 1/2 1/4)) #'>))
   (eqls (apply type '(1 1/2 1/3 1/4 1/5))
         (funcall bubble-sort (apply type '(1 1/3 1/5 1/2 1/4))
                  #'(lambda (m n) (< (denominator m) (denominator n)))) )
   (eqls (apply type '(1 1/2 1/3 1/4 1/5))
         (funcall bubble-sort (apply type '(1 1/3 1/5 1/2 1/4)) #'< :key #'denominator))
   (let ((a (apply type '(61 82 67 4 98 20 37 85))))
     (eqls (apply type '(4 20 37 61 67 82 85 98)) (funcall bubble-sort a #'<)))
   (eqls (apply type '("pung" "foo" "baz" "bar"))
         (funcall bubble-sort (apply type '("pung" "foo" "bar" "baz")) #'string>))
   (eqls (apply type '("Pung" "FOO" "baz" "BAR"))
         (funcall bubble-sort (apply type '("Pung" "FOO" "BAR" "baz")) #'string-greaterp))
   (eqls (apply type '("Pung" "FOO" "baz" "BAR"))
         (funcall bubble-sort (apply type '("Pung" "FOO" "BAR" "baz")) #'string> :key #'string-downcase))
   (eqls (apply type '("bar" "baz" "foo" "pung"))
         (funcall bubble-sort (apply type '("pung" "foo" "bar" "baz")) #'string<))
   (eqls (apply type '("BAR" "baz" "Foo" "pUNG"))
         (funcall bubble-sort (apply type '("pUNG" "Foo" "BAR" "baz")) #'string-lessp))
   (eqls (apply type '("foo" "bar" "baz"))
         (funcall bubble-sort (apply type '("foo" "bar" "baz")) #'(lambda (a b) (< (length a) (length b)))) ) ; Stable
   (eqls (apply type '("foo" "bar" "baz"))
         (funcall bubble-sort (apply type '("foo" "bar" "baz")) #'< :key #'length)) ; Stable
   (eqls (apply type '("foo" "bar" "baz"))
         (funcall bubble-sort (apply type '("foo" "bar" "baz")) #'(lambda (a b) (> (length a) (length b)))) ) ; Stable
   (eqls (apply type '("foo" "bar" "baz" "cat" "dog" "car" "cdr" "pung"))
         (funcall bubble-sort (apply type '("pung" "foo" "bar" "baz" "cat" "dog" "car" "cdr")) #'(lambda (a b) (< (length a) (length b)))) ) ; Stable
   (eqls (apply type '((z . 2) (k . 3) (p . 4) (a . 5) (b . 9)))
         (funcall bubble-sort (coerce '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)) type)
                  #'(lambda (a b) (< (cdr a) (cdr b)))) )
   (eqls (apply type '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)))
         (funcall bubble-sort (coerce '((B . 9) (A . 5) (K . 3) (P . 4) (Z . 2)) type)
                  #'(lambda (a b) (string< (string (car a)) (string (car b)))) ))
   (eqls (apply type '((a . 5) (b . 9) (k . 3) (p . 4) (z . 2)))
         (funcall bubble-sort (coerce '((B . 9) (A . 5) (K . 3) (P . 4) (Z . 2)) type)
                  #'string< :key (compose #'string #'car)))
   (dotimes (i 100 t)
     (let* ((a (case type
                 (vector (random-ordered-array))
                 (list (random-ordered-list))))
            (b (coerce (shuffle (coerce (copy-seq a) 'vector)) type)))
       (unless (and (eqls a (funcall bubble-sort b #'<))
                    (eqls (reverse a) (funcall bubble-sort b #'>)))
         (return nil)))) ))

;;;
;;;    Surprising timing results!!
;;;    Lists faster than arrays?!
;;;    - AREF is too genral?
;;;    - Vector is contiguous memory (random-access) but actual elts still follow pointer.
;;;    
;; (defvar *input* (shuffle (coerce (loop for i from 1 to 100000 collect i) 'vector)))
;; (defvar *input-list* (coerce *input* 'list))
;; (time (sort (copy-seq *input*) #'<))
;; Evaluation took:
;;   0.018 seconds of real time
;;   0.017554 seconds of total run time (0.017554 user, 0.000000 system)
;;   100.00% CPU
;;   55,986,585 processor cycles
;;   800,016 bytes consed
  
;; #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-2 (copy-seq *input*) #'<))
;; Evaluation took:
;;   68.266 seconds of real time
;;   68.261623 seconds of total run time (68.261623 user, 0.000000 system)
;;   99.99% CPU
;;   217,568,789,053 processor cycles
;;   800,016 bytes consed
  
;; #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-2a (copy-seq *input*) #'<))
;; Evaluation took:
;;   57.878 seconds of real time
;;   57.869804 seconds of total run time (57.869804 user, 0.000000 system)
;;   99.99% CPU
;;   184,464,673,797 processor cycles
;;   800,016 bytes consed
  
;; #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-2b (copy-seq *input*) #'<))
;; Evaluation took:
;;   28.201 seconds of real time
;;   28.199905 seconds of total run time (28.199905 user, 0.000000 system)
;;   100.00% CPU
;;   89,880,983,308 processor cycles
;;   800,016 bytes consed
  
;; #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
  
;; (time (bubble-sort-3 (copy-seq *input*) #'<))
;; Evaluation took:
;;   58.299 seconds of real time
;;   58.296839 seconds of total run time (58.295855 user, 0.000984 system)
;;   100.00% CPU
;;   185,806,856,481 processor cycles
;;   800,016 bytes consed
  
;; #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-4 (copy-seq *input*) #'<))
;; Evaluation took:
;;   61.596 seconds of real time
;;   61.593651 seconds of total run time (61.593651 user, 0.000000 system)
;;   100.00% CPU
;;   196,316,352,781 processor cycles
;;   832,704 bytes consed
  
;; #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
  



;; (time (sort (copy-seq *input-list*) #'<))
;; Evaluation took:
;;   0.016 seconds of real time
;;   0.016018 seconds of total run time (0.016018 user, 0.000000 system)
;;   100.00% CPU
;;   51,048,527 processor cycles
;;   1,604,848 bytes consed
  
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-list-1 (copy-seq *input-list*) #'<))
;; Evaluation took:
;;   32.059 seconds of real time
;;   32.057484 seconds of total run time (32.057484 user, 0.000000 system)
;;   99.99% CPU
;;   102,176,432,854 processor cycles
;;   1,587,680 bytes consed

;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-list-3 (copy-seq *input-list*) #'<))
;; Evaluation took:
;;   40.813 seconds of real time
;;   27.140667 seconds of total run time (27.136663 user, 0.004004 system)
;;   66.50% CPU
;;   12 lambdas converted
;;   130,075,546,040 processor cycles
;;   10,213,824 bytes consed

;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-list-4 (copy-seq *input-list*) #'<))
;; Evaluation took:
;;   27.912 seconds of real time
;;   27.908902 seconds of total run time (27.908902 user, 0.000000 system)
;;   99.99% CPU
;;   24 lambdas converted
;;   88,959,537,780 processor cycles
;;   16,855,760 bytes consed

;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29

;; (time (bubble-sort-list-5 (copy-seq *input-list*) #'<))
;; Evaluation took:
;;   27.709 seconds of real time
;;   27.708261 seconds of total run time (27.705262 user, 0.002999 system)
;;   100.00% CPU
;;   88,313,707,976 processor cycles
;;   17,561,312 bytes consed
  
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29



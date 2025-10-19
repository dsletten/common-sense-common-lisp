;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    In Lisp there is always more than one way to solve a problem.
;;;;    -- David Touretzky
;;;;
;;;;    Name:               exercises.lisp
;;;;
;;;;    Started:            Sat Oct  4 21:05:50 2025
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

(defpackage :exercises (:use :common-lisp :core :test) (:shadow :merge))

(in-package :exercises)

;;;
;;;    1.
;;;
(defun one-hundred-sum-array-p (a)
  (loop for i upto (floor (length a) 2)
        for j downfrom (1- (length a))
        unless (= 100 (+ (aref a i) (aref a j)))
        do (return nil)
        finally (return t)))

(defun one-hundred-sum-array-p (a)
  (loop for i from 0
        for j downfrom (1- (length a))
        until (> i j)
        unless (= 100 (+ (aref a i) (aref a j)))
        do (return nil)
        finally (return t)))

(deftest test-one-hundred-sum-array-p ()
  (check
   (not (one-hundred-sum-array-p #(2)))
   (one-hundred-sum-array-p #(2 98))
   (one-hundred-sum-array-p #(2 50 98))
   (not (one-hundred-sum-array-p #(2 51 98)))
   (not (one-hundred-sum-array-p #(2 50 99)))
   (one-hundred-sum-array-p #(20 87 4 96 13 80))
   (one-hundred-sum-array-p #(50))
   (one-hundred-sum-array-p #(50 50))
   (one-hundred-sum-array-p #(50 50 50))
   (one-hundred-sum-array-p #(50 50 50 50))
   (one-hundred-sum-array-p (coerce #[1 99] 'vector))
   (one-hundred-sum-array-p (coerce #[99 1] 'vector))
   (one-hundred-sum-array-p (coerce #[48 52] 'vector))
   (not (one-hundred-sum-array-p #(48 49 50 51)))
   (not (one-hundred-sum-array-p #(48 49 53 51 52)))) )

;;;
;;;    2.
;;;
(defun merge (a b)
  (loop with result = (make-array 20 :fill-pointer 0 :adjustable t)
        with i = 0
        with j = 0
        while (or (< i (length a)) (< j (length b)))
        if (= i (length a))
          do (vector-push-extend (aref b j) result)
             (incf j)
        else if (= j (length b))
          do (vector-push-extend (aref a i) result)
             (incf i)
        else if (< (aref b j) (aref a i))
          do (vector-push-extend (aref b j) result)
             (incf j)
        else 
          do (vector-push-extend (aref a i) result)
             (incf i)
        finally (return result)))

(defun merge (a b)
  (cond ((emptyp a) b)
        ((emptyp b) a)
        (t (let ((result (make-array 20 :fill-pointer 0 :adjustable t)))
             (labels ((merge-both (i j)
                        (cond ((= i (length a)) (merge-remainder b j))
                              ((= j (length b)) (merge-remainder a i))
                              ((< (aref b j) (aref a i))
                               (vector-push-extend (aref b j) result)
                               (merge-both i (1+ j)))
                              (t (vector-push-extend (aref a i) result)
                                 (merge-both (1+ i) j))))
                      (merge-remainder (a i)
                        (cond ((< i (length a))
                               (vector-push-extend (aref a i) result)
                               (merge-remainder a (1+ i)))
                              (t result))))
               (merge-both 0 0)))) ))

(deftest test-merge ()
  (check
   (equals #(1 2 3 4 5) (merge #() #(1 2 3 4 5)))
   (equals #(1 2 3 4 5) (merge #(1 2 3 4 5) #()))
   (equals #(1 2 3 4 5 6 7 8) (merge #(1 3 5 7) #(2 4 6 8)))
   (equals #(1 2 3 4 5 6 7 8 9) (merge #(1 3 5 7 9) #(2 4 6 8)))
   (equals #(1 2 3 4 5 6 7 8 10) (merge #(1 3 5 7) #(2 4 6 8 10)))
   (equals (merge #(2 4 6 8) #(1 3 5 7)) (merge #(1 3 5 7) #(2 4 6 8)))
   (equals (merge #(2 4 6 8) #(1 3 5 7 9)) (merge #(1 3 5 7 9) #(2 4 6 8)))
   (equals (merge #(2 4 6 8 10) #(1 3 5 7)) (merge #(1 3 5 7) #(2 4 6 8 10)))
   (equals #(1 2 3.0d0 3 4 5 6 7 8) (merge #(1 3d0 5 7) #(2 3 4 6 8)))) )

;;;
;;;    3.
;;;
;;;    另见 Slade ch. 6
;;;    ~/lisp/books/Slade/ch06/2009/original.lisp
;;;    ~/lisp/books/Slade/ch06/2009/naive.lisp
;;;    ~/lisp/books/Slade/ch06/2009/rabin-karp.lisp
;;;    Mastering Algorithms with Perl (ch. 9 pg. 364)
;;;    Intro to Algorithms ch. 32
;;;    String Algorithms in C
;;;    
;;;    This "matcher" follows Intro to Algorithms and returns indexes of all matches of PATTERN with TEXT.
;;;    This is different than Wengrow's simple boolean for any match.
;;;    
;; (defun naive-string-matcher (text pattern)
;;   (loop for s from 0 upto (- (length text) (length pattern))
;;         when (search pattern text :start2 s) collect s))

;;;
;;;    D'oh! We are basically trying to write SEARCH!?!?!
;;;    
;; (defun naive-string-matcher-0 (text pattern)
;;   (loop for s = 0 then (1+ shift)
;;         for shift = (search pattern text :start2 s)
;;         until (null shift)
;;         collect shift))

;;;
;;;    Need some extra logic for empty PATTERN!
;;;    
;; (defun naive-string-matcher-0 (text pattern)
;;   (let ((limit (- (length text) (length pattern))))
;;     (labels ((seek (s)
;;                (if (> s limit)
;;                    nil
;;                    (search pattern text :start2 s :test #'char=)))) 
;;       (loop for s = 0 then (1+ shift)
;;             for shift = (seek s)
;;             until (null shift)
;;             collect shift))))

(defun naive-string-matcher-0 (text pattern)
  (loop with limit = (- (length text) (length pattern))
        for s = 0 then (1+ shift)
        for shift = (and (<= s limit) (search pattern text :start2 s :test #'char=))
        until (null shift)
        collect shift))

(defun naive-string-matcher-1 (text pattern)
  (loop for s from 0 upto (- (length text) (length pattern))
        for e from (length pattern)
        when (search pattern text :start2 s :end2 e :test #'char=) collect s))

(defun naive-string-matcher-2 (text pattern)
  (loop for s from 0 upto (- (length text) (length pattern))
        for m = (mismatch pattern text :start2 s)
        when (or (null m) (= m (length pattern))) collect s))

(defun naive-string-matcher-3 (text pattern)
  (loop for s from 0 upto (- (length text) (length pattern))
        for e from (length pattern)
        when (string= pattern (subseq text s e)) collect s))

;;;
;;;    #1
;;;    
(defun naive-string-matcher-3a (text pattern)
  (loop for s from 0 upto (- (length text) (length pattern))
        for e from (length pattern)
        when (string= pattern text :start2 s :end2 e) collect s)) ; <---------------- !!!!

;; (defun naive-string-matcher-4 (text pattern)
;;   (loop for s from 0 upto (- (length text) (length pattern))
;;         nconc (loop for i below (length pattern)
;;                     for j from s
;;                     unless (char= (char pattern i) (char text j)) do (return nil)
;;                     finally (return (list s)))) )

(defun naive-string-matcher-4 (text pattern)
  (loop for s from 0 upto (- (length text) (length pattern))
        when (loop for i below (length pattern)
                   for j from s
                   always (char= (char pattern i) (char text j)))
        collect s))

;; (defun naive-string-matcher-5 (text pattern)
;;   (do ((shifts '())
;;        (s 0 (1+ s)))
;;       ((> s (- (length text) (length pattern))) (nreverse shifts))
;;     (do ((i 0 (1+ i))
;;          (j s (1+ j)))
;;         ((= i (length pattern)) (push s shifts))
;;       (unless (char= (char pattern i) (char text j))
;;         (return nil)))) )

(defun naive-string-matcher-5 (text pattern)
  (do ((n (length text))
       (m (length pattern))
       (shifts '())
       (s 0 (1+ s)))
      ((> s (- n m)) (nreverse shifts))
    (when (do ((i 0 (1+ i))
               (j s (1+ j)))
              ((= i m) t)
            (unless (char= (char pattern i) (char text j))
              (return nil)))
      (push s shifts))))

(defun naive-string-matcher-6 (text pattern)
  (labels ((outer (s shifts)
             (cond ((> s (- (length text) (length pattern))) (nreverse shifts))
                   (t (outer (1+ s) (inner s 0 s shifts)))) )
           (inner (s i j shifts)
             (cond ((= i (length pattern)) (cons s shifts))
                   ((char= (char pattern i) (char text j)) (inner s (1+ i) (1+ j) shifts))
                   (t shifts))))
    (outer 0 '())))

;;;
;;;    Compute lengths once!
;;;    
(defun naive-string-matcher-7 (text pattern)
  (let ((n (length text))
        (m (length pattern)))
    (labels ((outer (s shifts)
               (cond ((> s (- n m)) (nreverse shifts))
                     (t (outer (1+ s) (inner s 0 s shifts)))) )
             (inner (s i j shifts)
               (cond ((= i m) (cons s shifts))
                     ((char= (char pattern i) (char text j)) (inner s (1+ i) (1+ j) shifts))
                     (t shifts))))
      (outer 0 '()))) )

;;;
;;;    Clean up logic. (Compare 5 vs. 8)
;;;    
(defun naive-string-matcher-8 (text pattern)
  (let ((n (length text))
        (m (length pattern)))
    (labels ((outer (s shifts)
               (cond ((> s (- n m)) (nreverse shifts))
                     ((inner 0 s) (outer (1+ s) (cons s shifts)))
                     (t (outer (1+ s) shifts))))
             (inner (i j)
               (cond ((= i m) t)
                     ((char= (char pattern i) (char text j)) (inner (1+ i) (1+ j)))
                     (t nil))))
      (outer 0 '()))) )

(deftest test-naive-string-matcher ()
  (check
   (equal '(0) (naive-string-matcher "" ""))
   (not (naive-string-matcher "ffo" "foo"))
   (equal '(1) (naive-string-matcher "ffoo" "foo"))
   (equal '(0) (naive-string-matcher "foo" "foo"))
   (not (naive-string-matcher "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (equal '(36) (naive-string-matcher "peter piper picked a peck of pretty pickled peppers" "pickle"))
   (equal '(0 1) (naive-string-matcher "a" ""))
   (not (naive-string-matcher "" "a"))
   (equal '(12) (naive-string-matcher "Is this not pung?" "pung"))
   (equal '(12) (naive-string-matcher "Is this not pung?" "pung?"))
   (equal '(0 1 2) (naive-string-matcher "aaa" "a"))
   (equal '(2 6 18) (naive-string-matcher "ababc abc d efgab abcde" "abc"))
   (equal '(1 5 11) (naive-string-matcher "000010001010001" "0001"))))

;; (format t "~B~%" (expt 3 200))
;; 11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001

;;;
;;;    This is not the fastest! Built-in SEARCH...
;;;    
;; (time (dotimes (i 100000) (naive-string-matcher-0 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.400 seconds of real time
;;   0.399856 seconds of total run time (0.399856 user, 0.000000 system)
;;   [ Real times consist of 0.001 seconds GC time, and 0.399 seconds non-GC time. ]
;;   100.00% CPU
;;   1,274,377,079 processor cycles
;;   31,998,720 bytes consed
  
;; NIL

;; (time (dotimes (i 100000) (naive-string-matcher-1 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.884 seconds of real time
;;   0.884049 seconds of total run time (0.884049 user, 0.000000 system)
;;   100.00% CPU
;;   2,817,691,825 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

;; (time (dotimes (i 100000) (naive-string-matcher-2 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.763 seconds of real time
;;   0.763570 seconds of total run time (0.763570 user, 0.000000 system)
;;   [ Real times consist of 0.002 seconds GC time, and 0.761 seconds non-GC time. ]
;;   [ Run times consist of 0.001 seconds GC time, and 0.763 seconds non-GC time. ]
;;   100.13% CPU
;;   2,433,625,078 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

;; (time (dotimes (i 100000) (naive-string-matcher-3 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.584 seconds of real time
;;   0.584178 seconds of total run time (0.577232 user, 0.006946 system)
;;   [ Real times consist of 0.026 seconds GC time, and 0.558 seconds non-GC time. ]
;;   [ Run times consist of 0.023 seconds GC time, and 0.562 seconds non-GC time. ]
;;   100.00% CPU
;;   1,861,293,483 processor cycles
;;   1,036,511,872 bytes consed              <---------------------- Lots of CONSing due to SUBSEQ
  
;; NIL

;; Yowza! Fixed.
;; (time (dotimes (i 100000) (naive-string-matcher-3a "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.193 seconds of real time
;;   0.192298 seconds of total run time (0.192298 user, 0.000000 system)
;;   99.48% CPU
;;   612,931,442 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

;; (time (dotimes (i 100000) (naive-string-matcher-4 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.308 seconds of real time
;;   0.307755 seconds of total run time (0.307755 user, 0.000000 system)
;;   [ Real times consist of 0.001 seconds GC time, and 0.307 seconds non-GC time. ]
;;   [ Run times consist of 0.001 seconds GC time, and 0.307 seconds non-GC time. ]
;;   100.00% CPU
;;   981,065,294 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

;; (time (dotimes (i 100000) (naive-string-matcher-5 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.469 seconds of real time
;;   0.469614 seconds of total run time (0.464628 user, 0.004986 system)
;;   [ Real times consist of 0.008 seconds GC time, and 0.461 seconds non-GC time. ]
;;   [ Run times consist of 0.007 seconds GC time, and 0.463 seconds non-GC time. ]
;;   100.21% CPU
;;   1,494,573,757 processor cycles
;;   32,713,920 bytes consed
  
;; NIL

;; (time (dotimes (i 100000) (naive-string-matcher-6 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.526 seconds of real time
;;   0.525970 seconds of total run time (0.519979 user, 0.005991 system)
;;   [ Real times consist of 0.001 seconds GC time, and 0.525 seconds non-GC time. ]
;;   100.00% CPU
;;   1,676,369,453 processor cycles
;;   63,997,408 bytes consed
  
;; NIL

;; Replace REVERSE with NREVERSE:
;; (time (dotimes (i 100000) (naive-string-matcher-6 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.512 seconds of real time
;;   0.512278 seconds of total run time (0.512278 user, 0.000000 system)
;;   [ Real times consist of 0.001 seconds GC time, and 0.511 seconds non-GC time. ]
;;   100.00% CPU
;;   1,632,744,988 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

;; Calculate lengths once:
;; (time (dotimes (i 100000) (naive-string-matcher-7 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.324 seconds of real time
;;   0.323556 seconds of total run time (0.323556 user, 0.000000 system)
;;   100.00% CPU
;;   1,031,250,371 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

;; Cleaned up logic
;; (time (dotimes (i 100000) (naive-string-matcher-8 "11111110101011000011000111100001111101011000001000110100111101100001000011010100100110111101001110110111100110100001100101111111111010111001111011001011111100100010001110110000001101011011010000011111011001111011011110110111001001010011110101110001000100101101111111010111111110001111010101010111110001011000010100001" "0110")))
;; Evaluation took:
;;   0.296 seconds of real time
;;   0.295977 seconds of total run time (0.295977 user, 0.000000 system)
;;   100.00% CPU
;;   943,362,019 processor cycles
;;   31,998,704 bytes consed
  
;; NIL

(defun find-needle-0 (haystack needle)
  (search needle haystack :test #'char=)) ; D'oh!

(defun find-needle-1 (haystack needle)
  (loop for s from 0 upto (- (length haystack) (length needle))
        for e from (length needle)
        when (search needle haystack :start2 s :end2 e :test #'char=) do (return s)))

(defun find-needle-2 (haystack needle)
  (loop for s from 0 upto (- (length haystack) (length needle))
        for m = (mismatch needle haystack :start2 s)
        when (or (null m) (= m (length needle))) do (return s)))

(defun find-needle-3 (haystack needle)
  (loop for s from 0 upto (- (length haystack) (length needle))
        for e from (length needle)
        when (string= needle (subseq haystack s e)) do (return s)))

(defun find-needle-3a (haystack needle)
  (loop for s from 0 upto (- (length haystack) (length needle))
        for e from (length needle)
        when (string= needle haystack :start2 s :end2 e) do (return s)))

(defun find-needle-4 (haystack needle)
  (loop for s from 0 upto (- (length haystack) (length needle))
        when (loop for i below (length needle)
                   for j from s
                   always (char= (char needle i) (char haystack j)))
        return s))

;; (defun find-needle-5 (haystack needle)
;;   (do ((s 0 (1+ s)))
;;       ((> s (- (length haystack) (length needle))) nil)
;;     (do ((i 0 (1+ i))
;;          (j s (1+ j)))
;;         ((= i (length needle)) (return-from find-needle-5 s))
;;       (unless (char= (char needle i) (char haystack j))
;;         (return nil)))) )

(defun find-needle-5 (haystack needle)
  (do ((n (length haystack))
       (m (length needle))
       (s 0 (1+ s)))
      ((> s (- n m)) nil)
    (when (do ((i 0 (1+ i))
               (j s (1+ j)))
              ((= i m) t)
            (unless (char= (char needle i) (char haystack j))
              (return nil)))
      (return s))))

(defun find-needle-6 (haystack needle)
  (let ((n (length haystack))
        (m (length needle)))
    (labels ((outer (s)
               (cond ((> s (- n m)) nil)
                     ((inner 0 s) s)
                     (t (outer (1+ s)))) )
             (inner (i j)
               (cond ((= i m) t)
                     ((char= (char needle i) (char haystack j)) (inner (1+ i) (1+ j)))
                     (t nil))))
      (outer 0))))

(deftest test-find-needle ()
  (check
   (find-needle "" "")
   (not (find-needle "ffo" "foo"))
   (find-needle "ffoo" "foo")
   (find-needle "foo" "foo")
   (not (find-needle "peter piper picked a peck of pretty pickled peppers" "pickler"))
   (find-needle "peter piper picked a peck of pretty pickled peppers" "pickle")
   (find-needle "a" "")
   (not (find-needle "" "a"))
   (find-needle "Is this not pung?" "pung")
   (find-needle "Is this not pung?" "pung?")
   (find-needle "aaa" "a")
   (find-needle "ababc abc d efgab abcde" "abc")
   (find-needle "000010001010001" "0001")))




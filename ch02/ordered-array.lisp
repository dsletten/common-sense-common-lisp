;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               ordered-array.lisp
;;;;
;;;;   Started:            Sun Apr  6 19:17:49 2025
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

(defpackage :ordered-array
  (:use :common-lisp :core :test)
  (:nicknames :oa)
  (:export :read :search :add :delete :make-ordered-set :size)
  (:shadow :read :delete :search))

(in-package :ordered-array)

(defclass ordered-array ()
  ((store :initform (make-array 20 :fill-pointer 0 :adjustable t))
   (test :initform #'< :initarg :test)
   (key :initform #'identity :initarg :key)))

(defmethod print-object ((a ordered-array) stream)
  (print-unreadable-object (a stream :type t)
    (with-slots (store) a
      (format stream "~A [~D]" store (size a)))) )

(defun make-ordered-array (contents &key (key #'identity) (test #'<))
  (let ((ordered-array (make-instance 'ordered-array :key key :test test)))
    (loop for item in contents
          do (add ordered-array item)
          finally (return ordered-array))))

(defgeneric size (a)
  (:documentation "How many elements does A contain?"))
(defmethod size ((a ordered-array))
  (with-slots (store) a
    (length store)))

(defgeneric add (a obj)
  (:documentation "Add OBJ to A at the correct location if not already present."))
(defmethod add ((a ordered-array) obj)
  (labels ((extend (store)
             (unless (array-in-bounds-p store (fill-pointer store))
               (adjust-array store (* 2 (length store))))
             (incf (fill-pointer store)))
           (shift-up (store i)
             (setf (subseq store (1+ i)) (subseq store i)))
           (insert (store i)
             (assert (<= 0 i (length store)) () "Invalid index: ~A" i)
             (extend store)
             (shift-up store i)
             (setf (aref store i) obj)))
    (let ((index (search a obj)))
      (when (minusp index)
        (with-slots (store) a
          (insert store (1- (- index)))) ))))

(defgeneric read (a i)
  (:documentation "Read the value of element I from A."))
(defmethod read :around ((a ordered-array) i)
  (assert (<= 0 i (1- (size a))) () "Invalid index: ~A" i)
  (call-next-method))
(defmethod read ((a ordered-array) i)
  (with-slots (store) a
    (aref store i)))

(defgeneric delete (a obj)
  (:documentation "Destructively remove the object from A."))
(defmethod delete ((a ordered-array) obj)
  (let ((index (search a obj)))
    (unless (minusp index)
      (delete-at a index))))

(defgeneric delete-at (a i)
  (:documentation "Destructively remove the element at index I from A."))
(defmethod delete-at :around ((a ordered-array) i)
  (assert (<= 0 i (1- (size a))) () "Invalid index: ~A" i)
  (call-next-method))
(defmethod delete-at ((a ordered-array) i)
  (flet ((shift-down (store)
           (setf (subseq store i) (subseq store (1+ i)))) )
    (with-slots (store) a
      (let ((doomed (read a i)))
        (shift-down store)
        (decf (fill-pointer store))
        doomed))))

(defgeneric search (a obj)
  (:documentation "Locate index of OBJ within A. Return -(i + 1) if not present, where i is the index at which the OBJ would be found if present."))
(defmethod search ((a ordered-array) obj)
  (with-slots (store key test) a
    (binary-search store (funcall key obj) :test test :key key)))

;;;
;;;    The combination of KEY and TEST determines what "duplicates" mean for a given ordered array.
;;;    For example, this array treats any CONS with the same symbol as CAR as a duplicate entry:
;;;    (make-ordered-array '((c . 1) (c . 5) (a . 1) (b . 3) (b . 2)) :key #'car :test #'(lambda (a b) (string< (symbol-name a) (symbol-name b)))))
;;;    
;;;    Consequently, only 3 entries are accepted:
;;;    #<ORDERED-ARRAY #((A . 1) (B . 3) (C . 1)) [3]>
;;;    
;;;    A different arrangement of the inputs results in a different ordered array:
;;;    (make-ordered-array '((c . 5) (c . 1) (a . 1) (b . 2) (b . 3)) :key #'car :test #'(lambda (a b) (string< (symbol-name a) (symbol-name b))))
;;;    #<ORDERED-ARRAY #((A . 1) (B . 2) (C . 5)) [3]>
;;;
;;;    By contrast, this array does not use a KEY but rather a more complex TEST. Each CONS with different CAR/CDR is unique:
;;;    (make-ordered-array '((c . 1) (c . 5) (a . 1) (b . 3) (b . 2))                               
;;;                        :test #'(lambda (a b)                                                    
;;;                                  (flet ((primary (a b)                                          
;;;                                           (string< (symbol-name (car a)) (symbol-name (car b))))
;;;                                         (secondary (a b)                                        
;;;                                           (< (cdr a) (cdr b))))                                 
;;;                                    (cond ((primary a b) t)                                      
;;;                                          ((primary b a) nil)                                    
;;;                                          (t (secondary a b)))) ))
;;;
;;;    All of the inputs are inserted:
;;;    #<ORDERED-ARRAY #((A . 1) (B . 2) (B . 3) (C . 1) (C . 5)) [5]>
;;;

;; (make-ordered-array '((c . 1) (c . 5) (a . 1) (b . 3) (b . 2)) :test (partial* #'compound-compare (list (list #'string< (compose #'symbol-name #'car)) (list #'< #'cdr))))
;; #<ORDERED-ARRAY #((A . 1) (B . 2) (B . 3) (C . 1) (C . 5)) [5]>






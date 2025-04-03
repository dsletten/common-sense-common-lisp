;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               array-set.lisp
;;;;
;;;;   Started:            Tue Mar 18 21:42:18 2025
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
;;;;   The book has already begun to confuse interface with implementation.
;;;;   This version has different performance than the book's simplistic array-based
;;;;   implementation. In particular, INSERT and DELETE are as efficient as the earlier
;;;;   array shopping list. SEARCH in general is even faster. Certainly faster than the
;;;;   book's full linear scan. However, following an insertion or deletion work must be
;;;;   done to fix the INDEX hashtable. This rehashing doesn't need to take place after
;;;;   each insertion/deletion but rather simply before a SEARCH takes place. In
;;;;   particular, the INDEX will still reliably reflect an already present object prior
;;;;   to an attempt at insertion even if the _location_ of that object as indicated
;;;;   by the INDEX is out of date.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :array-set
  (:use :common-lisp :core :test)
  (:nicknames :as)
  (:export :add :read :search :insert :delete :make-array-set :size)
  (:shadow :read :delete :search))

(in-package :array-set)

(defclass array-set ()
  ((store :initform (make-array 20 :fill-pointer 0 :adjustable t))
   (index :initform (make-hash-table :test #'equal))
   (needs-rehash-p :initform nil)))

(defmethod print-object ((a array-set) stream)
  (print-unreadable-object (a stream :type t)
    (with-slots (store) a
      (format stream "~A [~D]" store (size a)))) )

(defun make-array-set (contents)
  (let ((array-set (make-instance 'array-set)))
    (loop for item in contents
          do (add array-set item)
          finally (return array-set))))

(defgeneric size (a)
  (:documentation "How many elements does A contain?"))
(defmethod size ((a array-set))
  (with-slots (store) a
    (length store)))

(defgeneric insert (a i obj)
  (:documentation "Insert OBJ into A at index I only if the index is valid and OBJ is not already present in A."))
(defmethod insert :around ((a array-set) i obj)
  (assert (<= 0 i (size a)) () "Invalid index: ~A" i)
  (call-next-method))
(defmethod insert ((a array-set) i obj)
  (flet ((extend (store)
           (unless (array-in-bounds-p store (fill-pointer store))
             (adjust-array store (* 2 (length store))))
           (incf (fill-pointer store)))
         (shift-up (store)
           (setf (subseq store (1+ i)) (subseq store i))))
    (with-slots (store index needs-rehash-p) a
      (unless (gethash obj index)
        (extend store)
        (shift-up store)
        (setf (aref store i) obj
              (gethash obj index) i
              needs-rehash-p t)))) )

;; (defmethod insert ((a array-set) i obj)
;;   (flet ((shift-up (store)
;;            (setf (subseq store (1+ i)) (subseq store i))))
;;     (with-slots (store index needs-rehash-p) a
;;       (unless (gethash obj index)
;;         (vector-push-extend nil store)
;;         (shift-up store)
;;         (setf (aref store i) obj
;;               (gethash obj index) i
;;               needs-rehash-p t)))) )

;;;
;;;    Don't need to rehash after ADD, but calling INSERT sets flag...
;;;    
(defgeneric add (a obj)
  (:documentation "Add OBJ to end of A only if OBJ is not already present in A."))
(defmethod add ((a array-set) obj)
  (insert a (size a) obj))

(defgeneric read (a i)
  (:documentation "Read the value of element I from A."))
(defmethod read :around ((a array-set) i)
  (assert (<= 0 i (1- (size a))) () "Invalid index: ~A" i)
  (call-next-method))
(defmethod read ((a array-set) i)
  (with-slots (store) a
    (aref store i)))

(defgeneric delete (a i)
  (:documentation "Destructively remove the element at index I from A."))
(defmethod delete :around ((a array-set) i)
  (assert (<= 0 i (1- (size a))) () "Invalid index: ~A" i)
  (call-next-method))
(defmethod delete ((a array-set) i)
  (flet ((shift-down (store)
           (setf (subseq store i) (subseq store (1+ i)))) )
    (with-slots (store index needs-rehash-p) a
      (let ((doomed (read a i)))
        (remhash doomed index)
        (shift-down store)
        (decf (fill-pointer store))
        (setf needs-rehash-p t)
        doomed))))

;; (defmethod delete ((a array-set) i)
;;   (flet ((shift-down (store)
;;            (setf (subseq store i) (subseq store (1+ i)))) )
;;     (with-slots (store index needs-rehash-p) a
;;       (let ((doomed (read a i)))
;;         (remhash doomed index)
;;         (shift-down store)
;;         (vector-pop store)
;;         (setf needs-rehash-p t)
;;         doomed))))

(defgeneric search (a obj)
  (:documentation "Locate index of OBJ within A. Return NIL if not present."))
(defmethod search :around ((a array-set) obj)
  (flet ((rehash (store index)
           (loop for i from 0
                 for elt across store
                 do (setf (gethash elt index) i))))
    (with-slots (needs-rehash-p store index) a
      (when needs-rehash-p
        (rehash store index)
        (setf needs-rehash-p nil))
      (call-next-method))))
(defmethod search ((a array-set) obj)
  (with-slots (index) a
    (gethash obj index)))

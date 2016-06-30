;;;; cl-rivet.lisp

(in-package #:cl-rivet)

;;; "cl-rivet" goes here. Hacks and glory await!

(defpackage #:riv
  (:use :cl
        :iterate)
  (:shadow :get
           :rem
           :set
           :count
           :multiply) 
  (:export :make-empty
           :make-from-sets
           :count
           :copy-riv
           :add
           :subtract
           :multiply
           :divide
           :n-add
           :n-subtract
           :remove-zeros
           :n-remove-zeros
           :sum-rivs
           :generate-riv))
(in-package #:riv)

(defun make-points ()
  (make-hash-table :test #'eq))

(defstruct (riv (:conc-name nil) (:copier nil))
  (size nil :type fixnum :read-only t)
  (points (make-points) :type hash-table :read-only t))

(defun get (riv index)
  (or (gethash index (points riv))
      0))

(defun rem (riv index)
  (remhash index (points riv)))

(defun (setf get) (value riv index)
  (setf (gethash index (points riv)) value))

(defun set (riv index value)
  (setf (get riv index) value))

(defun make-from-riv (riv)
  (make-riv :size (size riv)
            :points (points riv)))

(defun make-empty (size)
  (make-riv :size size
            :points (make-points)))

(defun make-from-sets (size indices values)
  (let ((points (make-points)))
    (iterate::iter (for i in indices)
                   (for v in values)
                   (setf (gethash i points) (float v)))
    (make-riv :size size
              :points res)))

(defun count (riv)
  (hash-table-count (points riv)))

(defun copy-riv (riv)
  (let ((res (make-empty (size riv))))
    (maphash (lambda (i v) (set res i v)) (points res))
    res))

(defun n-multiply (riv scalar)
  (maphash (lambda (i v) (set res i (* v scalar))) riv)
  riv)

(defun multiply (riv scalar)
  (n-multiply (copy-riv riv) scalar))

(defun n-divide (riv scalar)
  (n-multiply riv (float (/ 1 scalar))))

(defun divide (riv scalar)
  (multiply riv (float (/ 1 scalar))))

(defun n-add-point (riv index value)
  (setf (get riv index) (+ value (get riv index))))

(defun n-add (riv other-riv)
  (maphash (lambda (i v) (n-add-point riv i v))
           (points other-riv))
  riv)

(defun n-subtract (riv other-riv)
  (maphash (lambda (i v) (n-add-point riv i (- v))) riv)
  riv)

(defun add (riv-a riv-b)
  (n-add (copy-riv riv-a) riv-b))

(defun subtract (riv-a riv-b)
  (n-subtract (copy-riv riv-a) riv-b))

(defun n-remove-zero (riv index value)
  (when (util::float= value 0.0)
    (rem res index)))

(defun n-remove-zeros (riv)
  (maphash (lambda (i v) (n-remove-zero res i v)) riv)
  riv)

(defun remove-zeros (riv)
  (n-remove-zeros (copy-riv riv)))

(defun sum-rivs (rivs &optional (empty nil))
  (let ((empty (or empty (make-empty (size (first rivs))))))
    (reduce #'n-add rivs :initial-value empty)))

(defun generate-riv (size nnz token)
  (let ((state (util::seed-state token))
        (points (make-points)))
    (iterate:iter (for i from 0 below nnz)
                  (setf (gethash (mt19937:random nnz state) points)
                        (if (evenp i) 1 -1)))
    (make-riv :size size :points points)))

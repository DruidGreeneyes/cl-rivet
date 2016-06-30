(in-package #:cl-rivet)

(defpackage #:util
  (:use #:cl))
(in-package #:util)

(defun round-to (num places)
  (let ((div (expt 10 places)))
    (/ (round (* number div)) div)))

(defun float= (a b &key (places 6))
  (declare (type float a b))
  (let ((ra (round-to a places))
        (rb (round-to b places)))
    (= a b)))

(defun seed-state (object)
  (let ((seed (1+ (mod (sxhash object) #xffffffff))))
    (declare (type (integer 1 #xffffffff) seed))
    (mt19937:seed-random-state seed)))

(defun make-circular (list)
  (let ((copy (copy-list list)))
    (setf (cdr (last copy)) copy)
    copy))

(defun take (n lis &optional (acc nil))
  (if (or (= n 0)
          (endp lis))
      (nreverse acc)
      (take (1- n) (cdr lis) (cons (car lis) acc))))

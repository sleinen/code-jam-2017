;; -*- mode: common-lisp -*-

(defpackage :c (:use :common-lisp))

(in-package :c)

(defun C (name)
  (let ((in-file-name (format nil "~A.in" name))
	(out-file-name (format nil "~A.out" name)))
    (with-open-file (in in-file-name)
      (with-open-file (out out-file-name
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
	(let ((n (read in)))
	  (dotimes (i n)
	    (let* ((n (read in))
		   (k (read in)))
	      (multiple-value-bind (min max)
		  (minmax n k)
		(format out "Case #~D: ~D ~D~%"
			(+ i 1) max min)))))))))
(defun minmax (n k)
  (minmax-1 n (- k 1) 1 0))

(defun minmax-1 (n k a b)
  (cond ((< k b)
	 (split (+ n 1)))
	((< k (+ a b))
	 (split n))
	(t
	 (let ((half-n (floor (- n 1) 2)))
	   (if (oddp n)
	       (minmax-1 half-n
			 (- k (+ a b))
			 (+ (* 2 a) b)
			 b)
	       (minmax-1 half-n
			 (- k (+ a b))
			 a
			 (+ a (* 2 b))))))))

(defun split (n)
  (let ((half (floor n 2)))
    (if (oddp n)
	(values half half)
	(values (1- half) half))))

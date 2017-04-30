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
	(let ((num (read in)))
	  (dotimes (i num)
	    (let* ((n (read in)) (k (read in))
		   (u (coerce (read in) 'double-float))
		   (p (make-array (list n))))
	      (dotimes (k n)
		(setf (aref p k) (coerce (read in) 'double-float)))
	      (format out "Case #~D: ~F~%"
		      (+ i 1) (trained-prob n k u p)))))))))

(defun trained-prob (n k u p)
  (if (= n k)
      (trained-prob-easy n u p)
      (progn
	(warn "I cannot do this")
	(aref p 0))))

(defun trained-prob-easy (n u p)
  (setq p (sort p #'<))
  (tp-1 n u p (smallest-prefix-size p)))

(defun zero-enough-p (x)
  (< (abs x) 1d-18))

(defun tp-1 (n u p end)
  (if (zero-enough-p u)
      (reduce #'* p)
      (let ((incr (/ u end))
	    (max (if (< end n) (aref p end) 1d0)))
	(setq incr (min incr (- max (aref p 0))))
	(dotimes (k end)
	  (incf (aref p k) incr))
	(tp-1 n (- u (* incr end)) p (smallest-prefix-size p)))))

(defun smallest-prefix-size (p)
  (1+ (position (aref p 0) p :from-end t #+foo :test #+foo #'similar-enough-p)))

(defun similar-enough-p (a b)
  (or (< (abs (- a b)) 1d-10)
      (< (abs (/ (- a b) (+ a b))) 1d-10)))

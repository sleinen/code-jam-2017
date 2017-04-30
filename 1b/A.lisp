;; -*- mode: common-lisp -*-

(defpackage :a (:use :common-lisp))

(in-package :a)

(defun A (name)
  (let ((in-file-name (format nil "~A.in" name))
	(out-file-name (format nil "~A.out" name)))
    (with-open-file (in in-file-name)
      (with-open-file (out out-file-name
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
	(let ((n (read in)))
	  (dotimes (i n)
	    (let* ((d (read in))
		   (n (read in))
		   (horses '()))
	      (dotimes (k n)
		(push (cons (read in) (read in))
		      horses))
	      (let ((speed (maxspeed d horses)))
		(format out "Case #~D: ~,6F~%"
			(+ i 1) speed)))))))))

(defun maxspeed (d horses)
  (/ d (reduce #'max
	       (mapcar #'(lambda (horse)
			   (let ((dstart (car horse))
				 (s (cdr horse)))
			     (/ (- (+ d 0d0) dstart) s)))
		       horses))))

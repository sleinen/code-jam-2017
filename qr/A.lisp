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
	    (let ((str
		   (do ((chars '() (cons c chars))
			(c (read-char in) (read-char in)))
		       ((char= c #\space)
			(coerce chars 'string)))))
	      (let ((k (read in)))
		(let ((nsteps (nsteps str k)))
		  (format out "Case #~D: ~A~%"
			  (+ i 1) nsteps))))))))))

(defun nsteps (vec k)
  (nsteps-1 vec k 0 0))

(defun nsteps-1 (vec k start nflips)
  (if (>= start (length vec))
      nflips
      (ecase (char vec start)
	(#\+ (nsteps-1 vec k (1+ start) nflips))
	(#\-
	 (if (> (+ start k) (length vec))
	     :IMPOSSIBLE
	     (dotimes (i k (nsteps-1 vec k (1+ start) (1+ nflips)))
	       (setf (char vec (+ start i))
		     (ecase (char vec (+ start i))
		       (#\+ #\-)
		       (#\- #\+)))))))))

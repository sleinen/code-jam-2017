;; -*- mode: common-lisp -*-

(defpackage :d (:use :common-lisp))

(in-package :d)

(defun D (name)
  (let ((in-file-name (format nil "~A.in" name))
	(out-file-name (format nil "~A.out" name)))
    (with-open-file (in in-file-name)
      (with-open-file (out out-file-name
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
	(let ((n (read in)))
	  (dotimes (i n)
	    (let* ((n (read in)) (m (read in))
		   (array (make-array (list n n)
				      :initial-element nil)))
	      (dotimes (ii m)
		(let* ((sym (read-char in))
		       (must-be-space (read-char in))
		       (rii (1- (read in)))
		       (cii (1- (read in))))
		  (assert (char= must-be-space #\Space))
		  (setf (aref array rii cii) sym)))
	      ;(format t "~S" array)
	      (format out "Case #~D: ~A~%"
		      (+ i 1) m))))))))

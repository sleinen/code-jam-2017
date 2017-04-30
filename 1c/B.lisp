;; -*- mode: common-lisp -*-

(defpackage :b (:use :common-lisp))

(in-package :b)

(defstruct (activity
	     (:conc-name a-))
  start
  end)

(defun a-duration (a) (mod (- (a-end a) (a-start a)) 1440))

(defun B (name)
  (let ((in-file-name (format nil "~A.in" name))
	(out-file-name (format nil "~A.out" name)))
    (with-open-file (in in-file-name)
      (with-open-file (out out-file-name
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
	(let ((n (read in)))
	  (dotimes (i n)
	    (let* ((nac (read in)) (naj (read in))
		   (ac (make-array (list nac)))
		   (aj (make-array (list naj))))
	      (dotimes (k nac)
		(let* ((start (read in)) (end (read in)))
		  (setf (aref ac k) (make-activity :start start :end end))))
	      (dotimes (k naj)
		(let* ((start (read in)) (end (read in)))
		  (setf (aref aj k) (make-activity :start start :end end))))
	      (setq ac (sort ac #'(lambda (a b) (< (a-start a) (a-start b)))))
	      (setq aj (sort aj #'(lambda (a b) (< (a-start a) (a-start b)))))
	      (format out "Case #~D: ~D~%"
		      (+ i 1) (min-exchanges ac aj)))))))))

(defun total-a-duration (a)
  (reduce #'+ a :key #'a-duration))

(defun fit-in-one-half-circle-p (ac)
  (dotimes (k (length ac) nil)
    (when (fit-1 ac k)
      (return t))))

(defun fit-1 (ac k)
  (let ((min (a-start (aref ac k)))
	(max (a-end (aref ac (mod (- k 1) (length ac))))))
    (<= (mod (- max min) 1440) 720)))

(defun min-exchanges (ac aj)
  (cond ((and (<= (length ac) 1) (<= (length aj) 1)) 2)
	((and (= (length ac) 2) (= (length aj) 0))
	 (if (fit-in-one-half-circle-p ac)
	     2
	     4))
	((and (= (length aj) 2) (= (length ac) 0))
	 (min-exchanges aj ac))
	(t
	 (warn "total duration of C's activities: ~D" (total-a-duration ac))
	 (warn "total duration of J's activities: ~D" (total-a-duration aj))
	 0)))

;; -*- mode: common-lisp -*-

(defpackage :a (:use :common-lisp))

(in-package :a)

(defstruct (pancake)
  r
  h
  side-surface)

(defun A (name)
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
		   (p (make-array (list n))))
	      (dotimes (j n)
		(let* ((r (read in)) (h (read in)))
		  (setf (aref p j) (make-pancake :r r :h h))))
	      (format out "Case #~D: ~,9F~%"
		      (+ i 1) (max-surface n k p)))))))))

(defun max-surface (n k p)
  (setq p (sort p #'(lambda (p1 p2)
		      (or (> (pancake-r p1) (pancake-r p2))
			  (and (= (pancake-r p1) (pancake-r p2))
			       (> (pancake-h p1) (pancake-h p2)))))))
  (dotimes (i n)
    (let ((pancake (aref p i)))
      (let ((h (pancake-h pancake))
	    (r (pancake-r pancake)))
	(setf (pancake-side-surface pancake)
	      (* h 2 r pi)))))
  (let ((best 0))
    (do ((i 0 (1+ i)))
	((> (+ k i) n) best)
      (let ((surf0 (+ (circular-surface (aref p i))
		      (pancake-side-surface (aref p i)))))
	(setq best (max best (+ surf0 (sum-side-surfaces n (- k 1) p (+ i 1)))))))))

(defun circular-surface (p)
  (* pi (expt (pancake-r p) 2)))

(defun sum-side-surfaces (n k p start)
    (setq p (sort (copy-seq (subseq p start))
		  #'(lambda (p1 p2) (> (pancake-side-surface p1) (pancake-side-surface p2)))))
    (let ((best-pancakes (subseq p 0 k)))
      (reduce #'+ best-pancakes :key #'pancake-side-surface))))

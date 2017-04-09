;; -*- mode: common-lisp -*-

(defpackage :b (:use :common-lisp))

(in-package :b)

(defun B (file)
  (with-open-file (in file)
    (let ((n (read in)))
      (dotimes (k n)
	(let ((number-as-string (read-line in)))
	  (let ((number-as-vec
		 (map 'vector #'(lambda (c) (parse-integer (string c)))
		      number-as-string)))
	    (format t "Case #~D: ~{~D~}~%"
		    (+ k 1)
		    (coerce (lt number-as-vec) 'list))))))))
(defun lt (v)
  (if (<= (length v) 1)
      v
      (lt-1 v 0 (- (length v) 1))))

(defun lt-1 (v i stop)
  (cond ((= i stop)
	 v)
	((<= (aref v i) (aref v (+ i 1)))
	 (lt-1 v (+ i 1) stop))
	(t
	 (lt (maybe-trim (next-lower-v v i))))))

(defun maybe-trim (v)
  (if (= (aref v 0) 0)
      (maybe-trim (subseq v 1))
      v))

(defun next-lower-v (v k)
  (let ((vk (aref v k)))
    (let ((new-vk (- vk 1)))
      (setf (aref v k) new-vk)
      (do ((i (+ k 1) (+ i 1)))
	  ((>= i (length v)) v)
	(setf (aref v i) 9)))))

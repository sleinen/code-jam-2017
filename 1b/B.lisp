;; -*- mode: common-lisp -*-

(defpackage :b (:use :common-lisp))

(in-package :b)

(defun B (name)
  (let ((in-file-name (format nil "~A.in" name))
	(out-file-name (format nil "~A.out" name)))
    (with-open-file (in in-file-name)
      (with-open-file (out out-file-name
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede)
	(let ((ntests (read in)))
	  (dotimes (i ntests)
	    (let* ((n (read in))
		   (r (read in))
		   (o (read in))
		   (y (read in))
		   (g (read in))
		   (b (read in))
		   (v (read in)))
	      (format out "Case #~D: ~A~%"
		      (+ i 1)
		      (solve n r o y g b v)))))))))

(defun solve (n r o y g b v)
  (if (not (zerop (+ o g v)))
      'IMPOSSIBLE
      (let ((result '()))
	(dotimes (i n)
	  (let ((max (if (endp result)
			 (max r y b)
			 (case (first result)
			   ((R) (max y b))
			   ((Y) (max r b))
			   ((B) (max r y))))))
	    (cond ((and (= max r) (not (and result (eq (first result) 'R))))
		   (push 'R result) (decf r))
		  ((and (= max y) (not (and result (eq (first result) 'Y))))
		   (push 'Y result) (decf y))
		  ((= max b)
		   (when (and result (eq (first result) 'B))
		       (return nil))
		   (push 'B result) (decf b)))))
	(if (or (not result)
		(eq (first result) (first (last result))))
	    'IMPOSSIBLE
	    (format nil "~{~A~}" (reverse result))))))

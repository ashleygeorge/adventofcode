(require "asdf")

(defun overlaps-p (first second)
  (or
   (<= (car second) (cadr first) (cadr second))
   (<= (car second) (car first) (cadr second))))

(defun extract-pair (pairstr)
  (mapcar #'parse-integer (uiop:split-string pairstr :separator "-")))

(defun extract-pairs (assignments)
  (uiop:split-string assignments :separator ","))

(defun read-lines (stream)
  (let ((line (read-line stream nil)))
	(cond
	  ((null line) '())
	  ('t 
	(cons
	 (destructuring-bind (a b) (mapcar #'extract-pair (extract-pairs line))
	   (if (or (overlaps-p a b) (overlaps-p b a)) 1.0 0.0))
	 (read-lines stream))))))

(defun read-problem-data (filename)
  (with-open-file (stream filename)
	(apply #'+ (read-lines stream))))

(read-problem-data "input")

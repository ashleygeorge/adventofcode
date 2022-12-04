(require "asdf")

(defun contains-p (first second)
  (<= (car first) (car second) (cadr second) (cadr first)))

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
	   (if (or (contains-p a b) (contains-p b a)) 1.0 0.0))
	 (read-lines stream))))))

(defun read-problem-data (filename)
  (with-open-file (stream filename)
	(apply #'+ (read-lines stream))))

(read-problem-data "input")

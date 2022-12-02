(require "asdf")
(ql:quickload "alexandria")

(defun read-while-not-empty (stream)
  (let ((line (read-line stream nil)))
	(cond
	  ((null line) '())
	  ((= (length (string-trim " " line)) 0) '())
	  ((cons (parse-integer line) (read-while-not-empty stream))))))

(defun read-max (stream &optional (running-maximum 0))
  (let ((next (read-while-not-empty stream)))
	(cond
	  ((null next) running-maximum)
	  ('t (read-max stream (max running-maximum (apply #'+ next)))))))

(defun read-top-n (stream n &optional (running-maximum-list `(0 0 0)))
  (let ((next (read-while-not-empty stream)))
	(cond
	  ((null next) running-maximum-list)
	  ('t (read-top-n stream 3 (subseq (sort (cons (apply #'+ next) running-maximum-list ) #'>) 0 n))))))

		
(defun read-problem-data (filename)
  (with-open-file (stream filename)
	(apply #'+ (read-top-n stream 3))))


(read-problem-data "input")

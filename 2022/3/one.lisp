(require "asdf")

(defun string-to-list (s)
  (coerce s 'list))

(defun char-to-priority (c)
  (let ((code (char-code c)))
	(cond
	  ((< 64 code 91) (+ 26 (- code 64)))
	  ((< 96 code 123) (- code 96))
	  ('t (error "character ~S code ~S out of range" c code)))))

(defun string-halves (s)
  (list
   (string-to-list (subseq s 0 (/ (length s) 2)))
   (string-to-list (subseq s (/ (length s) 2)))))

(defun score-rucksack (r)
  (apply #'+ (mapcar #'char-to-priority (remove-duplicates (apply #'intersection (string-halves r))))))

(defun read-lines (stream)
  (let ((line (read-line stream nil)))
	(cond
	  ((null line) '())
	  ('t (cons (score-rucksack line) (read-lines stream))))))
	  
(defun read-problem-data (filename)
  (with-open-file (stream filename)
	(apply #'+ (read-lines stream))))


(read-problem-data "input")

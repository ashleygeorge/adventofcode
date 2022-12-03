(require "asdf")

(defun string-to-list (s)
  (coerce s 'list))

(defun char-to-priority (c)
  (let ((code (char-code c)))
	(cond
	  ((< 64 code 91) (+ 26 (- code 64)))
	  ((< 96 code 123) (- code 96))
	  ('t (error "character ~S code ~S out of range" c code)))))

(defun score-group (g1 g2 g3)
  (apply #'+ (mapcar #'char-to-priority (remove-duplicates (intersection g1 (intersection g2 g3))))))

(defun read-groups (stream)
  (let ((l1 (string-to-list (read-line stream nil)))
		(l2 (string-to-list (read-line stream nil)))
		(l3 (string-to-list (read-line stream nil))))
	(cond
	  ((null l1) ())
	  ('t (cons (score-group l1 l2 l3) (read-groups stream))))))

(defun read-problem-data (filename)
  (with-open-file (stream filename)
	(apply #'+ (read-groups stream))))

(read-problem-data "input")

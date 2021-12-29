(require "asdf")
(ql:quickload "alexandria")

(defun parse-calls (line)
  (mapcar #'parse-integer (uiop:split-string line :separator ",")))

(defun parse-board-line (line)
  (mapcar #'parse-integer
		  (remove-if-not #'(lambda (i) (> (length i) 0))
						 (uiop:split-string line :separator " "))))

(defun parse-board (stream)
  (let ((line (read-line stream nil)))
	(cond
	  ((null line) nil)
	  ((> (length line) 0) (cons (parse-board-line line) (parse-board stream)))
	  ('t '()))))

(defun parse-boards (stream)
  (let ((board (parse-board stream)))
	(if (null board) '()
		(cons board (parse-boards stream)))))

(defun board-row (board i)
  (if (= i 0) (car board) (board-row (cdr board) (- i 1))))

(defun board-column (board i)
  (mapcar (alexandria:curry #'nth i) board))

(defun board-transpose (board)
  (mapcar (alexandria:curry #'board-column board) (alexandria:iota (length board))))

(defun vector-windex (calls vec)
  (apply #'max
		 (mapcar
		  (alexandria:curry
		   #'(lambda (calls value)
			   (position value calls))
		   calls)
		  vec)))

(defun vector-unmarked-sum (calls vec)
  (apply #'+ (mapcar (alexandria:curry #'(lambda (calls value) (if (position value calls) 0 value)) calls) vec)))

(defun board-unmarked-sum (calls board)
  (apply #'+ (mapcar (alexandria:curry #'vector-unmarked-sum calls) board)))

(defun board-windex (calls board)
  (min 
   (apply #'min (mapcar (alexandria:curry #'vector-windex calls) board))
   (apply #'min (mapcar (alexandria:curry #'vector-windex calls) (board-transpose board)))))

(defun boards-winner (calls boards)
  (let ((winners (mapcar (alexandria:curry #'board-windex calls) boards)))
	(nth (position (apply #'min winners) winners) boards)))

(defun boards-last-winner (calls boards)
  (let ((winners (mapcar (alexandria:curry #'board-windex calls) boards)))
	(nth (position (car (sort (copy-seq winners) #'>)) winners) boards)
  ))

(defun read-problem-data (filename)
  (with-open-file (stream filename)
	(let*
		((calls (parse-calls (read-line stream)))
		 (skip (read-line stream))
		 (boards (parse-boards stream))
		 (winner (boards-winner calls boards))
		 (winning-call (nth (board-windex calls winner) calls))
		 (winning-sum (board-unmarked-sum (subseq calls 0 (+ 1 (board-windex calls winner))) winner))
		 (last-winner (boards-last-winner calls boards))
		 (last-winning-call (nth (board-windex calls last-winner) calls))
		 (last-winning-sum (board-unmarked-sum (subseq calls 0 (+ 1 (board-windex calls last-winner))) last-winner))
		 )
	  (declare (ignore skip))
	  (pprint winner)
	  (pprint winning-call)
	  (pprint winning-sum)
	  (pprint (* winning-call winning-sum))
	  (pprint last-winner)
	  (pprint last-winning-call)
	  (pprint last-winning-sum)
	  (pprint (* last-winning-call last-winning-sum))
	  )))

(read-problem-data "input")





	

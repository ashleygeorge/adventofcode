(require "asdf")

(defun convert-shape (shape)
  (cond
	((equal shape "A") "X")
	((equal shape "B") "Y")
	((equal shape "C") "Z")
	('t (error "Unexpected input ~S, not A B or C" shape))))
	

(defun shape-score (shape)
  (cond
	((equal shape "X") 1)
	((equal shape "Y") 2)
	((equal shape "Z") 3)
	('t (error "Unexpected input ~S, not X Y or Z" shape))))


(defun round-score (a b)
  (cond
	((and (equal a "C") (equal b "X")) 6)
	((and (equal a "A") (equal b "Y")) 6)
	((and (equal a "B") (equal b "Z")) 6)
	((and (equal a "A") (equal b "X")) 3)
	((and (equal a "B") (equal b "Y")) 3)
	((and (equal a "C") (equal b "Z")) 3)
	('t 0)))

(defun expected-move (a b)
  (cond
	((and (equal a "A") (equal b "X")) "Z")
	((and (equal a "A") (equal b "Z")) "Y")
	((and (equal a "B") (equal b "X")) "X")
	((and (equal a "B") (equal b "Z")) "Z")
	((and (equal a "C") (equal b "X")) "Y")
	((and (equal a "C") (equal b "Z")) "X")
	((equal b "Y") (convert-shape a))
	('t (error "Unexpected inputs ~S ~S" a b))))

(defun score-rounds (stream)
  (let ((line (read-line stream nil)))
	(if (null line) 0
	(destructuring-bind (a b) (uiop:split-string line)
	  (+ (shape-score b) (round-score a b) (score-rounds stream))))))

(defun score-rounds-by-expected-move (stream)
  (let ((line (read-line stream nil)))
	(if (null line) 0
		(destructuring-bind (a b) (uiop:split-string line)
		  (+ (shape-score (expected-move a b)) (round-score a (expected-move a b)) (score-rounds-by-expected-move stream))))))

(defun one ()
  (with-open-file (stream "input")
	(score-rounds stream)))

(defun two ()
  (with-open-file (stream "input")
	(score-rounds-by-expected-move stream)))

(one)

(two)

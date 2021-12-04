(require "asdf")
(defvar *problem-input* (uiop:read-file-lines "input"))

(defun bits-to-integer (bs)
  (parse-integer bs :radix 2))

(defun nth-bit (bits n)
  (ash (logand (ash 1 n) bits) (- n)))

(defun add-bits (bits counts pos)
  (if counts
	  (cons (+ (car counts) (nth-bit bits (- pos 1))) (add-bits bits (cdr counts) (- pos 1)))
	  '()))

(defun count-bits (bitstrings count-list)
  (if bitstrings
	  (let ((bits (bits-to-integer (car bitstrings))))
		(count-bits (cdr bitstrings) (add-bits bits count-list (length (car bitstrings)))))
	  count-list
	  ))

(defun boolean-to-integer (b)
  (if b 1 0))

(defun common-bit (bits-type)
  (cond ((equalp bits-type 'most) nil)
		((equalp bits-type 'least) t)))

(defun filter-by-counting-bits (bitstrings pos bits-type)
  (let ((curpos (- pos 1))
		(n (length bitstrings))
		(bitcounts (reverse (count-bits bitstrings (make-list (length (car bitstrings)) :initial-element 0)))))
	(if (equalp (length bitstrings) 1)
		(car bitstrings)
		(filter-by-counting-bits
		 (remove-if-not #'(lambda (bs)
							(equalp
							 (nth-bit (bits-to-integer bs) curpos)
							 (if (>= (car (nthcdr curpos bitcounts)) (/ n 2))
								 (boolean-to-integer (not (common-bit bits-type)))
								 (boolean-to-integer (common-bit bits-type)))))
						bitstrings)
		 curpos bits-type))))


;; result
(*
 (bits-to-integer (filter-by-counting-bits *problem-input* (length (car *problem-input*)) 'most))
 (bits-to-integer (filter-by-counting-bits *problem-input* (length (car *problem-input*)) 'least)))
  


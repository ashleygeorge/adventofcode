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

(defun bit-array-to-integer (bits)
  (if bits
	  (+ (ash (car bits) (- (length bits) 1)) (bit-array-to-integer (cdr bits)))
	  0))

(defun gamma-rate (bitstrings)
  (let ((threshold (/ (length bitstrings) 2))
		(bitcounts (count-bits bitstrings (make-list (length (car bitstrings)) :initial-element 0))))
	(bit-array-to-integer (mapcar #'(lambda (x) (if (> x threshold) 1 0)) bitcounts))))

(defun epsilon-rate (bitstrings)
  (let ((threshold (/ (length bitstrings) 2))
		(bitcounts (count-bits bitstrings (make-list (length (car bitstrings)) :initial-element 0))))
	(bit-array-to-integer (mapcar #'(lambda (x) (if (> x threshold) 0 1)) bitcounts))))

;; result
(* (gamma-rate *problem-input*) (epsilon-rate *problem-input*))




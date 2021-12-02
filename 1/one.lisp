(require "asdf")
(defvar *problem-input* (uiop:read-file-lines "input"))

(defun detect-increases (number-list)
  (loop for (a b) on number-list
		collect (> (- (if b b a) a) 0)))

(defun string-list-to-integers (slist)
  (if slist
	  (cons (parse-integer (car slist)) (string-list-to-integers (cdr slist)))
	  ()))

(defun count-increases (input)
  (loop for x in (detect-increases
				  (string-list-to-integers input))
		sum (if x 1 0)))

(count-increases *problem-input*)

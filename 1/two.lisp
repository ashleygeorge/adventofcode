(require "asdf")
(defvar *problem-input* (uiop:read-file-lines "input"))
(defvar *sample-input* '(199 200 208 210 200 207 240 269 260 263))

(defun detect-increases (number-list)
  (loop for (a b c d) on number-list
		collect (let (
					  (a (or a 0))
					  (b (or b 0))
					  (c (or c 0))
					  (d (or d 0)))
				  (> (- (+ b c d) (+ a b c)) 0))))

(defun string-list-to-integers (slist)
  (if slist
	  (cons (parse-integer (car slist)) (string-list-to-integers (cdr slist)))
	  ()))

(defun count-increases (input)
  (loop for x in (detect-increases input)
		sum (if x 1 0)))

(count-increases *sample-input*)
(count-increases (string-list-to-integers *problem-input*))

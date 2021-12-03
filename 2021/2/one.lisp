(require "asdf")
(defvar *problem-input* (uiop:read-file-lines "input"))

(defun update-position (hpos depth command count)
  (let ((c (parse-integer count)))
  (cond ((equalp command "forward") (list (+ hpos c) depth))
		((equalp command "down") (list hpos (+ depth c)))
		((equalp command "up") (list hpos (- depth c))))))

(defun process-commands (command-list hpos depth)
  (if (null command-list)
	  (* hpos depth)
	  (destructuring-bind (command count) (uiop:split-string (car command-list))
		(destructuring-bind (hpos_ depth_) (update-position hpos depth command count)
		  (process-commands (cdr command-list) hpos_ depth_)))))


(process-commands *problem-input* 0 0)


  
		 

(require "asdf")
(defvar *problem-input* (uiop:read-file-lines "input"))

(defun update-position (hpos depth aim command count)
  (let ((c (parse-integer count)))
  (cond ((equalp command "forward") (list (+ hpos c) (+ depth (* aim c)) aim))
		((equalp command "down") (list hpos depth (+ aim c)))
		((equalp command "up") (list hpos depth (- aim c))))))

(defun process-commands (command-list hpos depth aim)
  (if (null command-list)
	  (* hpos deptgh)
	  (destructuring-bind (command count)
		  (uiop:split-string (car command-list))
		(destructuring-bind (hpos_ depth_ aim_)
			(update-position hpos depth aim command count)
		  (process-commands (cdr command-list) hpos_ depth_ aim_)))))


(process-commands *problem-input* 0 0 0)


  
		 

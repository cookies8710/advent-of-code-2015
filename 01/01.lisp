#!/usr/bin/sbcl --script

(defvar *input* (read-line (open "input")))

(defun solution (str)
  (let ((flr 0) (i 1) basement)
	  (loop for ch across str
		do (progn
			 (if (char= ch #\() (incf flr) (decf flr))
			 (if (and (= flr -1) (not basement)) (setf basement i))
			 (incf i)))
	  (list flr basement)))


(format t "~{Part 1: ~a, Part 2: ~a~%~}" (solution *input*))

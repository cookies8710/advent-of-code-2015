#!/usr/bin/sbcl --script

(defparameter *dirs* (make-hash-table))
(setf (gethash #\^ *dirs*) '(0 1))
(setf (gethash #\v *dirs*) '(0 -1))
(setf (gethash #\< *dirs*) '(-1 0))
(setf (gethash #\> *dirs*) '(1 0))

(defun visit (ht coords)
  (multiple-value-bind (current present) (gethash coords ht)
    (if present
      (setf (gethash coords ht) (1+ current))
      (setf (gethash coords ht) 1))))

(defun move-and-visit (visited current dir)
  (setf current (mapcar #'+ current dir))
  (visit visited current)
  current)

(let ((current '(0 0)) 
      (visited (make-hash-table :test #'equal))
      (input (read-line(open "input"))))
  (visit visited current)
  (loop for chr across input do
        (setf current (move-and-visit visited current (gethash chr *dirs*))))
  (format t "Number of houses Santa visited (part 1): ~a~%" (hash-table-count visited)))

(let ((santa '(0 0)) 
      (robo '(0 0))
      (issanta t)
      (visited (make-hash-table :test #'equal))
      (input (read-line(open "input"))))
  (visit visited santa)
  (loop for chr across input do
        (if issanta
          (progn
            (setf santa (move-and-visit visited santa (gethash chr *dirs*)))
            (setf issanta nil))
          (progn
            (setf robo (move-and-visit visited robo (gethash chr *dirs*)))
            (setf issanta t))))
  (format t "Number of houses Santa and Robosanta visited (part 2): ~a~%" (hash-table-count visited)))

#!/usr/bin/sbcl --script

(defun has-double-letter (str)
  (let ((a (subseq str 1))
        (b (subseq str 0 (1- (length str)))))
    (some #'eql a b))) 

(defun is-vowel (chr)
  (position chr "aeiou"))

(defun is-nice (str)
  (let ((nice t))
    ; at least 3 vowels
    (setf nice (>= (count-if #'is-vowel str) 3))
    ; at least 1 double letter
    (setf nice (and nice (has-double-letter str)))
    ; doesnt have ab, cd, pq, xy
    (dolist (no '("ab" "cd" "pq" "xy"))
      (setf nice (and nice (not (search no str)))))
    nice))

(defun has-repeating-with-another-between (str)
  (let ((a (subseq str 2))
        (b (subseq str 0 (- (length str) 2))))
    (some #'eql a b))) 

(defun is-nice2 (str)
  (let ((nice nil))
  ; some 2 letter group repeats itself (no overlap)
  (dotimes (i (- (length str) 2))
    (let ((group (subseq str i (+ 2 i)))
          (rst (subseq str (+ 2 i))))
    (setf nice (or nice (search group rst)))))

  ; a repeated letter with a letter between occurrences
  (setf nice (and nice (has-repeating-with-another-between str)))
  nice))
  
(defun count-nice-in-file (file nice-predicate message)
 (let ((in (open file))
      (nice 0))
  (loop for line = (read-line in nil)
        while line do
        (if (funcall nice-predicate line) (incf nice)))
  (format t message nice)))

(count-nice-in-file "input" #'is-nice "There are ~a nice strings (Part 1)~%")
(count-nice-in-file "input" #'is-nice2 "There are ~a nice strings (Part 2)~%")

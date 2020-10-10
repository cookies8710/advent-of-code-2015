#!/usr/bin/sbcl --script

;; finds indices of all occurrences of `chr` in `str`
(defun position-all (str chr)
    (do 
      ((a 0) (pos nil)) ; var def
      ((not a) (reverse pos)) ; end-form: until !a, result-form is reversed pos

      (setf a (position chr str :start (1+ a)))
      (if a (setf pos (cons a pos)))))

;; splist string `str` by character `chr`
(defun split (str chr)
  (let ((prev 0) (parts nil))
    (dolist (pos (position-all str chr))
      (setf parts (cons (subseq str prev pos) parts))
      (setf prev (1+ pos)))
    (setf parts (cons (subseq str prev) parts))
    (reverse parts)))

(defun maximum (lst)
  (loop for i in lst maximizing i))

(defun minimum (lst)
  (loop for i in lst minimizing i))

;; rotates list
(defun rotate (lst)
  (cons (car (last lst)) (butlast lst)))

(defun prism-half-area (dims)
  (mapcar #'* dims (rotate dims)))

(defun paper (dims)
  (let ((area (prism-half-area dims)))
  (+ (minimum area) (* 2 (reduce #'+ area)))))

(defun ribbon (dims)
  (let ((volume (reduce #'* dims)) (ma (maximum dims)))
    (+ volume (* 2 (- (reduce #'+ dims) ma))))) ; ribbon is Volume + 2 * (a + b) where a, b are the smallest sides

(let ((in (open "input")) 
      (total-paper 0) 
      (total-ribbon 0))
  (loop for line = (read-line in nil)
        while line do 
        (let ((dims (mapcar #' parse-integer (split line #\x))))
          (setf total-ribbon (+ total-ribbon (ribbon dims)))
          (setf total-paper (+ total-paper (paper dims)))))
  (close in)
  (format t "Total sq. feet of paper (Part 1): ~a~%Total length of ribbon (Part 2): ~a~%" total-paper total-ribbon))

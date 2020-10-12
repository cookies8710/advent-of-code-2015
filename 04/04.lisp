#!/usr/bin/sbcl --script

(defparameter *S* (vector 7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22 5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20 4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23 6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))

(defparameter *K* (make-array 64 :fill-pointer 0))
(defvar *two32* 1)
(dotimes (i 32) 
  (setf *two32* (* 2 *two32*)))
(dotimes (i 64) 
  (vector-push (floor (* *two32* (abs (sin (* 1.0d0 (1+ i)))))) *K*))

(defun as-int (vec)
  (let ((result 0))
    (dotimes (i (length vec))
      (setf result (+ (* 256 result) (elt vec (- 3 i)))))
    result))

(defun left-rotate (n i)
  (bor32 (ash n i) (ash n (- i 32))))

(defun bnot (i)
  (+ #x100000000 (boole boole-nand i i)))

(defun band32 (a b)
  (boole boole-and #xffffffff (boole boole-and a b)))
(defun bor32 (a b)
  (boole boole-and #xffffffff (boole boole-ior a b)))
(defun bxor32 (a b)
  (boole boole-and #xffffffff (boole boole-xor a b)))

(defun as-hex8b (n)
  (reverse (if (> n 15) 
    (write-to-string n :base 16)
    (concatenate 'string "0" (write-to-string n :base 16)))))

(defun little-endian (n)
  (let ((result "")) 
    (dotimes (i 4)
      (setf result (concatenate 'string (as-hex8b (boole boole-and #xff n)) result))
      (setf n (ash n -8)))
    (reverse result)))

(defun md5 (str)
  (let ((message (map 'vector #'char-int str))
        (M (make-array 64 :fill-pointer 0))
        (a0 #x67452301)
        (b0 #xefcdab89)
        (c0 #x98badcfe)
        (d0 #x10325476))
    (dotimes (i (length message)) (vector-push (elt message i) M)) 
    (vector-push 128 M)
    (dotimes (i (- 56 (length M))) (vector-push 0 M))
    (vector-push (* 8 (length str)) M)
    (dotimes (i 7) (vector-push 0 M))
    (let ((A a0)
          (B b0)
          (C c0)
          (D d0))
      (dotimes (i 64)
        (let ((F 0) (g 0))
          (cond 
            ((< i 16) (progn
                        (setf F (bor32 (band32 B C) (band32 D (bnot B))))
                        (setf g i)))
            ((< i 32) (progn
                        (setf F (bor32 (band32 D B) (band32 (bnot D) C)))
                        (setf g (mod (+ 1 (* 5 i)) 16))))
            ((< i 48) (progn
                        (setf F (bxor32 B (bxor32 C D)))
                        (setf g (mod (+ 5 (* 3 i)) 16))))
            (t (progn
                        (setf F (bxor32 C (bor32 B (bnot D))))
                        (setf g (mod (* 7 i) 16)))))
          
          (setf F (+ F A (elt *K* i) (as-int (subseq M (* 4 g) (* 4 (1+ g))))))
          (setf F (boole boole-and #xffffffff F))
          (setf A D)
          (setf D C)
          (setf C B)
          (setf B (+ B (left-rotate F (elt *S* i))))
          (setf B (boole boole-and #xffffffff B))))
      
     (setf a0 (+ a0 A))
     (setf b0 (+ b0 B))
     (setf c0 (+ c0 C))
     (setf d0 (+ d0 D))

     (reduce #'(lambda (x y) (concatenate 'string x y)) (list (little-endian a0)(little-endian b0)(little-endian c0)(little-endian d0))))))

(defun starts-with (prefix str)
  (= (length prefix) (mismatch prefix str)))

; 5 zeros
(do ((i 0) msg found)
    (found)

    (if (= 0 (mod i 10000)) (format t "~a~%" i))
    (setf msg (concatenate 'string "ckczppom" (write-to-string i)))
    (setf found (starts-with "00000" (md5 msg)))
    (if found (format t "Found: ~a - ~a -> ~a~%" i msg (md5 msg)))
    (incf i))

; 6 zeros
(do ((i 0) msg found)
    (found)

    (if (= 0 (mod i 10000)) (format t "~a~%" i))
    (setf msg (concatenate 'string "ckczppom" (write-to-string i)))
    (setf found (starts-with "000000" (md5 msg)))
    (if found (format t "Found: ~a - ~a -> ~a~%" i msg (md5 msg)))
    (incf i))

(format t "Done~%") 

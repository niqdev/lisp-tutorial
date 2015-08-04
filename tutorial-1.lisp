; LISP Tutorial 1: Basic LISP Programming
; (load "tutorial-1.lisp")

; ---------- exercise 1.1 ----------
; The N'th triangular number is defined to be 1 + 2 + 3 + ... + N.
; Alternatively, we could give a recursive definition of triangular number as follows:
; T(n) = 1 if n = 1
; T(n) = n + T(n-1) if n > 1
; Use the recursive definition to help you implement a linearly recursive function
; (triangular N) that returns the N'th triangular number.

; type: (triangular 6)
; step1: (defun triangular (N) (if (= N 1) "a" "b"))
; step2: (defun triangular (N) (if (= N 1) 1 (+ N 1)))
(defun triangular (N) (if (= N 1) 1 (+ N (triangular (- N 1)))))
(trace triangular)

; ---------- exercise 1.2 ----------
; Write down a recursive definition of B^E (assuming that both B and E are non-negative integers).
; Then implement a linearly recursive function (power B E) that computes BE.
; if E == 0, B^E = 1
; if E == 1, B^E = B
; otherwise B^E = B * B^(E−1)

; type: (power 2 6)
(defun power (B E) (cond
  ((= E 0) 1)
  ((= E 1) B)
  (t (* B (power B (- E 1))))
))
(trace power)

; ---------- exercise 1.3 ----------
; The Binomial Coefficient B(n, r) is the coefficient of the term x^r in the binormial expansion of (1 + x)^n.
; For example B(4, 2) = 6 because (1 + x)^4 = 1 + 4x + 6x^2 + 4x^3 + x^4.
; The Binomial Coefficient can be computed using the Pascal Triangle formula:
; B(n, r) = 1 if r = 0 or r = n
; B(n, r) = B(n - 1, r - 1) + B(n - 1, r) otherwise
; Implement a doubly recursive function (binomial N R) that computes the binomial coefficient B(N, R).

; type: (binomial 4 2)
(defun binomial (N R)
  (if (or (zerop R) (= R N)) 1
  (+ (binomial (- N 1) (- R 1)) (binomial (- N 1) R))
))
(trace binomial)

; ---------- exercise 1.4 ----------
; Implement a linearly recursive function (sum L) which computes the sum of all numbers in a list L.
; Compare your solution with the standard pattern of structural recursion.

; type: (sum '(2 8 4 6))
(defun sum (L) (if (null L) 0 (+ (first L) (sum (rest L)))))
(trace sum)

; ---------- exercise 1.5 ----------
; LISP has a built-in function (last L) that returns a the last cons structure in a given list L.
; Implement your own version of last using linear recursion. You may assume that (last nil) returns nil.
; Compare your implementation with the standard pattern of structural recursion.

; type (last-item '(a b c d)) or (last-item '(1 2 3))
(defun last-item (L) (if (null L) nil (last (rest L))))
(trace last-item)

; ---------- exercise 1.6 ----------
; LISP defines a function (butlast L) that returns a list containing the same elements in L except for the last one.
; Implement your own version of butlast using linear recursion. You may assume that (butlast nil) returns nil.

; type: (but-last '(1 2 3 4 5))
(defun but-last (L) (cond
  ((null L) nil)
  ((null (cdr L)) nil)
  ((cons (first L) (but-last (rest L))))))
(trace but-last)

; ---------- exercise 1.7 ----------
; Give a linearly recursive implementation of union and difference.

; type: (list-union '(0 1 2 3 4 9) '(2 3 4 5 6 7 8))
(defun list-union (L1 L2) (cond
  ((null L1) L2)
  ((not (member (first L1) L2)) (cons (first L1) (list-union (rest L1) L2)))
  (t (list-union (rest L1) L2)) ))
(trace list-union)

; (list-difference '(0 1 2 3 4 9) '(2 3 4 5 6 7 8))
(defun list-difference (L1 L2) (cond
  ((null L1) nil)
  ((member (first L1) L2) (cons (first L1) (list-difference (rest L1) L2)))
  (t (list-difference (rest L1) L2)) ))
(trace list-difference)

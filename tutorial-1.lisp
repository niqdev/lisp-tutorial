;;; (load "tutorial-1.lisp")

;;; ---------- exercise 1.1 ----------
;;; The N'th triangular number is defined to be 1 + 2 + 3 + ... + N.
;;; Alternatively, we could give a recursive definition of triangular number as follows:
;;; T(n) = 1 if n = 1
;;; T(n) = n + T(n-1) if n > 1
;;; Use the recursive definition to help you implement a linearly recursive function
;;; (triangular N) that returns the N'th triangular number.

;; type: (triangular 6)
; step1: (defun triangular (N) (if (= N 1) "a" "b"))
; step2: (defun triangular (N) (if (= N 1) 1 (+ N 1)))
(defun triangular (N) (if (= N 1) 1 (+ N (triangular (- N 1)))))
(trace triangular)

;;; ---------- exercise 1.2 ----------
;;; Write down a recursive definition of B^E (assuming that both B and E are non-negative integers).
;;; Then implement a linearly recursive function (power B E) that computes BE.
;;; if E == 0, B^E = 1
;;; if E == 1, B^E = B
;;; otherwise B^E = B * B^(E−1)

;; type: (power 2 6)
(defun power (B E) (cond
  ((= E 0) 1)
  ((= E 1) B)
  (t (* B (power B (- E 1))))
))
(trace power)

;;; ---------- exercise 1.3 ----------
;;; The Binomial Coefficient B(n, r) is the coefficient of the term x^r in the binormial expansion of (1 + x)^n.
;;; For example B(4, 2) = 6 because (1 + x)^4 = 1 + 4x + 6x^2 + 4x^3 + x^4.
;;; The Binomial Coefficient can be computed using the Pascal Triangle formula:
;;; B(n, r) = 1 if r = 0 or r = n
;;; B(n, r) = B(n - 1, r - 1) + B(n - 1, r) otherwise
;;; Implement a doubly recursive function (binomial N R) that computes the binomial coefficient B(N, R).

;; type (binomial 4 2)
(defun binomial (N R)
  (if (or (zerop R) (= R N)) 1
  (+ (binomial (- N 1) (- R 1)) (binomial (- N 1) R))
))
(trace binomial)

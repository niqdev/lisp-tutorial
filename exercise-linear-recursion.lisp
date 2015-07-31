;;; (load "exercise-linear-recursion.lisp")

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

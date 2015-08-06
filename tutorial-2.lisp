; LISP Tutorial 2: Advanced Functional Programming in LISP
; (load "tutorial-2.lisp")

; example 1
; type: (list-reverse '(1 2 3 4 5))
(defun list-reverse (L) (list-reverse-aux L nil))
(defun list-reverse-aux (L A)
  (if (null L) A (list-reverse-aux (rest L) (cons (first L) A))))
(trace list-reverse)
(trace list-reverse-aux)

; example 2
; type: (fast-factorial 137)
(defun fast-factorial (N) (fast-factorial-aux N 1))
(defun fast-factorial-aux (N A)
  (if (= N 1) A (fast-factorial-aux (- N 1) (* N A))))
(trace fast-factorial)
(trace fast-factorial-aux)

;; Compilers for functional programming languages usually implement
;; tail-recursive call optimizations which automatically translate
;; a certain kind of linear recursion into efficient iterations.
;; A linear recursive function is tail-recursive if the result of each recursive call
;; is returned right away as the value of the function.

; ---------- exercise 2.1 ----------
; Recall that the N'th triangular number is defined to be 1 + 2 + 3 + ... + N.
; Give a tail-recursive implementation of the function (fast-triangular N) which returns the N'th triangular number.
(defun fast-triangular (N) (fast-triangular-aux N 1))
(defun fast-triangular-aux (N A)
  (if (= N 1) A (fast-triangular-aux (- N 1) (+ N A))))
(trace fast-triangular)
(trace fast-triangular-aux)

; ---------- exercise 2.2 ----------
; Give a tail-recursive implementation of the function (fast-power B E)
; that raises B to the power E (assuming that both B and E are non-negative integers).
(defun fast-power (B E) (fast-power-aux B E 1))
(defun fast-power-aux (B E A)
  (if (= E 0) A (fast-power-aux B (- E 1) (* B A))))
(trace fast-power)
(trace fast-power-aux)

; ---------- exercise 2.3 ----------
; Give a tail-recursive implementation of the function (fast-list-length L),
; which returns the length of a given list L.

; type (slow-list-length '(1 a 2 b c 3 d 4))
(defun slow-list-length (L) (if (null L) 0 (1+ (slow-list-length (rest L)))))
(trace slow-list-length)

(defun fast-list-length (L) (fast-list-length-aux L 0))
(defun fast-list-length-aux (L A)
  (if (null L) A (fast-list-length-aux (rest L) (+ A 1))))
(trace fast-list-length)
(trace fast-list-length-aux)

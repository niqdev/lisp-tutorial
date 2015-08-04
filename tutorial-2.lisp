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

; ---------- exercise 2.2 ----------
; Give a tail-recursive implementation of the function (fast-power B E)
; that raises B to the power E (assuming that both B and E are non-negative integers).

; ---------- exercise 2.3 ----------
; Give a tail-recursive implementation of the function (fast-list-length L),
; which returns the length of a given list L.

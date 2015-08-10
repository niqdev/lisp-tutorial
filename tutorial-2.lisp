; LISP Tutorial 2: Advanced Functional Programming in LISP
; (load "tutorial-2.lisp")

; ---------- example 1 ----------
; type: (list-reverse '(1 2 3 4 5))
(defun list-reverse (L) (list-reverse-aux L nil))
(defun list-reverse-aux (L A)
  (if (null L) A (list-reverse-aux (rest L) (cons (first L) A))))
(trace list-reverse)
(trace list-reverse-aux)

; ---------- example 2 ----------
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

; ---------- example 3 ----------
(defun repeat (F N X)
  "Repeat applying function F on object X for N times."
  (if (zerop N) X (repeat F (1- N) (funcall F X))))
(trace repeat)

; type: (repeat #'triple 4 1)
; type: (repeat (function triple) 4 1)
(defun triple (N) (* 3 N))

; type: (repeat #'prepend-word 5 nil)
; type: (repeat (function prepend-word) 5 nil)
(defun prepend-word (L) (cons 'lisp L))

;type: (list-nth 5 '(a b c d e f g h i j))
(defun list-nth (N L) (first (repeat #'rest N L)))

; ---------- example 4 ----------
; using lambda (triple)
(repeat #'(lambda (N) (* 3 N)) 4 1)

; using lambda (prepend-word)
(repeat #'(lambda (L) (cons 'lisp L)) 5 nil)

; ---------- exercise 2.4 ----------
; Define a function (apply-func-list L X) so that, given a list L of functions and an object X,
; apply-func-list applies the functions in L to X in reversed order.
; For example, the following expression are equivalent
; (apply-func-list (list #'double #'list-length #'rest) '(1 2 3))
; (double (list-length (rest '(1 2 3))))

; (triple (list-length (rest '(1 2 3))))
; (apply-func-list (list #'triple #'list-length #'rest) '(1 2 3))
(defun apply-func-list (L X)
  (if (null L) X (funcall (first L) (apply-func-list (rest L) X)) ))
(trace apply-func-list)

; note that (car (cons x y)) returns x and (cdr (cons x y)) returns y
; type: (reversed '(1 2 3 4 5))
(defun reversed (L)
  (if (null (cdr L)) (car L) (cons (reversed (rest L)) (first L)) ))
(trace reversed)

; ---------- exercise 2.5 ----------
; Use apply-func-list to compute the following:

; (2.5.a) 10 times the fourth element of the list (10 20 30 40 50)
(apply-func-list (list
  #'(lambda (E) (* 10 E))
  #'(lambda (L) (first (repeat #'rest 3 L)))
  ) '(10 20 30 40 50))

; (2.5.b) the third element of the second element in the list ((1 2) (3 4 5) (6))
(apply-func-list (list #'third #'second) '((1 2) (3 4 5) (6)))

; (2.5.c) the difference between 10 and the length of (a b c d e f)
(apply-func-list (list #'(lambda (N) (- 10 N)) #'fast-list-length) '(a b c d e f))

; (2.5.d) a list containing a list containing the symbol blah
(apply-func-list (list #'list #'(lambda (L) (cons 'blah L))) nil)

; ---------- example 5 ----------
; Given a list L of numbers, return a list containing the elements of L multiplied by 3
; type: (triple-list-elements '(1 2 3 4 5))
(defun triple-list-elements (L)
  (if (null L) nil (cons (triple (first L)) (triple-list-elements (rest L))) ))
(trace triple-list-elements)

; Given a list L of lists, return a list containing the reversal of L's members
; type: (reverse-list-elements '((1 2 3) (a b c) (4 5 6) (d e f)))
(defun reverse-list-elements (L)
  (if (null L) nil (cons (reverse (first L)) (reverse-list-elements (rest L))) ))
(trace reverse-list-elements)

; Apply function F to every element of list L, and return a list containing the results
; type: (mapfirst #'triple '(1 2 3 4 5))
; type: (mapfirst #'reverse '((1 2 3) (a b c) (4 5 6) (d e f)))
(defun mapfirst (F L)
  (if (null L) nil (cons (funcall F (first L)) (mapfirst F (rest L))) ))
(trace mapfirst)

; --> built-in MAPCAR
(mapcar #'butlast '((1 2 3) (a b c) (4 5 6) (d e f)))

; ---------- example 6 ----------
; Given a list L of numbers, return the leftmost even member
; type: (find-even '(3 5 7 8 5 4))
(defun find-even (L) (cond
  ((null L) nil)
  ((evenp (first L)) (first L))
  ((find-even (rest L))) ))

; ---------- exercise 2.6 ----------
; Implement a function that, when given a list L of lists, return a non-empty member of L
; type: (find-member '(nil nil (1 2 3) (4 5)))
(defun find-member (L) (cond
  ((null L) nil)
  ((not (null (first L))) (first L))
  ((find-member (rest L))) ))

; ---------- example 7 ----------
; Find the leftmost element of list L that satisfies predicate P
; type: (list-find-if #'evenp '(3 5 7 8 5 4))
; type: (list-find-if #'(lambda (L) (not (null L))) '(nil nil (1 2 3) (4 5)))
(defun list-find-if (P L) (cond
  ((null L) nil)
  ((funcall P (first L)) (first L))
  ((list-find-if P (rest L))) ))

; --> built-in FIND-IF
; (find-if #'evenp '(3 5 7 8 5 4))
(find-if #'(lambda (L) (not (null L))) '(nil nil (1 2 3) (4 5)))

; ---------- exercise 2.7 ----------
; (2.7.a) Use find-if to define a function that searches among
; a list of lists for a member that has length at least 3
(find-if #'(lambda (L) (>= (fast-list-length L) 3)) '((c) (a b) (1 2 3 4) (5 6 7)))

; (2.7.b) Use find-if to define a function that searches among
; a list of lists for a member that contains an even number of elements
(find-if #'(lambda (L) (evenp (fast-list-length L))) '((1 2 3) (e) (a b c) (4 5)))

; (2.7.c) Use find-if to define a function that searches among
; a list of numbers for a member that is divisible by three
(find-if #'(lambda (X) (zerop (rem X 3))) '(2 5 7 4 6 8 3))

; ---------- example 8 ----------

; Remove all members of L that has length less than three
; type: (remove-short-list '((d e) (1 2 3 4) (9) (a b c) nil (4 5 6 7 8)))
(defun remove-short-list (L) (cond
  ((null L) nil)
  ((< (fast-list-length (first L)) 3) (remove-short-list (rest L)))
  ((cons (first L) (remove-short-list (rest L)))) ))

; Remove all members of L that is an even number
; type: (remove-even '(2 3 8 10 7 5 6 3 12))
(defun remove-even (L) (cond
  ((null L) nil)
  ((evenp (first L)) (remove-even (rest L)))
  ((cons (first L) (remove-even (rest L)))) ))

; --> built-in REMOVE-IF
(remove-if #'(lambda (L) (< (fast-list-length L) 3))
  '((d e) (1 2 3 4) (9) (a b c) nil (4 5 6 7 8)))

(remove-if #'(lambda (L) (evenp L)) '(2 3 8 10 7 5 6 3 12))

; ---------- exercise 2.8 ----------
; Observe the recurring pattern in remove-short-lists and remove-even,
; and implement your own version of remove-if
; type: (list-remove-if #'(lambda (L) (< (fast-list-length L) 3)) '((d e) (1 2 3 4) (9) (a b c)))
; type: (list-remove-if #'(lambda (L) (evenp L)) '(2 3 8 10 7 5 6 3 12))
(defun list-remove-if (P L) (cond
  ((null L) nil)
  ((funcall P (first L)) (list-remove-if P (rest L)))
  ((cons (first L) (list-remove-if P (rest L)))) ))

; ---------- example 9   ----------
; Compute the intersection of lists L1 and L2.
(defun list-intersection (L1 L2)
  (remove-if #'(lambda (X) (not (member X L2))) L1))

; Use remove-if and lambda abstractions to implement list-difference
(defun list-difference (L1 L2)
  (remove-if #'(lambda (X) (member X L2)) L1))

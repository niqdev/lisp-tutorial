;;; to load functions type after prompt [1]>:
;;; (load "utils-example.lisp")

;; example usage: (double 5)
(defun double(x) (* 2 x))

;; example usage: (factorial 8)
(defun factorial (N) (if (= 1 N) 1 (* N (factorial (- N 1)))))
; turn debug on
(trace factorial)

#lang racket

(define lines (file->lines "01.input"))
(define nums (foldl (lambda (l ns) (if (non-empty-string? l) (cons (cons (string->number l) (car ns)) (cdr ns)) (cons '() ns))) '(()) lines))
(define sums (map (lambda (a) (apply + a)) nums))
(println (apply max sums))

(define top3 (take (sort sums >) 3))
(println (apply + top3))

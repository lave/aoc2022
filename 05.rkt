#lang racket

(require "common.rkt")

(define (parse-crates crates)
  (define transposed (transpose crates))
  (define lines-stack (filter (lambda (l) (not-space (car l))) transposed))
  (define stacks (map (lambda (l) (reverse (takef (cdr l) not-space))) lines-stack))
  stacks
)

(define (not-space c)
  (not (equal? #\space c))
)

(define (parse-move s)
  (define ts (string-split s))
  (map string->number (list (list-ref ts 1) (list-ref ts 3) (list-ref ts 5)))
)

(define (make-move bulk? move crates)
  (define-values (n index-from index-to) (apply values move))

  (define stack-from (list-ref crates (- index-from 1)))
  (define stack-from-new (drop stack-from n))

  (define crates-to-move (take stack-from n))

  (define stack-to (list-ref crates (- index-to 1)))
  (define stack-to-new (append (if bulk? crates-to-move (reverse crates-to-move)) stack-to))

  (define crates1 (list-set crates (- index-from 1) stack-from-new))
  (define crates2 (list-set crates1 (- index-to 1) stack-to-new))
  crates2
)

(define (tops crates)
  (list->string (map car crates))
)


(define-values (lines-crates lines-moves) (splitf-at (file->lines "05.input") non-empty-string?))
(define crates (parse-crates (reverse lines-crates)))
(define moves (map parse-move (cdr lines-moves)))
;(println crates)
;(println moves)

(define tops1 (tops (foldl (curry make-move #f) crates moves)))
(displayln tops1)

(define tops2 (tops (foldl (curry make-move #t) crates moves)))
(displayln tops2)

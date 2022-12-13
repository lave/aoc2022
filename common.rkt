#lang racket

(provide bool->number empty-string? split-by split-by-separator string-split-at map-1st map-2nd transpose)

(define (bool->number b) (if b 1 0))

(define (empty-string? s) (not (non-empty-string? s)))

(define (split-by lst n)
  (if (not (empty? lst)) (cons (take lst n) (split-by (drop lst n) n)) '())
)

(define (split-by-separator lst is-separator?)
  (reverse (map reverse
    (foldl
      (lambda (element result)
        (if (is-separator? element)
          (cons '() result)
          (cons (cons element (car result)) (cdr result))
        ))
      '(()) lst
    )))
)

(define (string-split-at s i)
  (define m (quotient (string-length s) 2))
  (list (substring s 0 i) (substring s i))
)

(define (map-1st f pair)
  (list (f (car pair)) (cadr pair))
)

(define (map-2nd f pair)
  (list (car pair) (f (cadr pair)))
)

(define (transpose matrix)
  (apply map list (map string->list matrix))
)

#lang racket

(provide empty-string? split-by split-by-separator string-split-at transpose)

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

(define (transpose matrix)
  (apply map list (map string->list matrix))
)
 

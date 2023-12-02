#lang racket

(require "common.rkt")

(define (parse-pair pair)
  (map parse-line pair)
)

(define (parse-line str)
  (read (open-input-string (string-replace (string-replace (string-replace str "," " ") "[" "(") "]" ")")))
)

(define (compare-pairs pair idx)
  (if (< (apply compare pair) 0) idx 0)
)

; <0 - l<r
; =0 - l=r
; >0 - l>r
(define (compare l r)
  (case (list (number? l) (number? r))
    ['(#t #t) (- l r)]
    ['(#t #f) (compare (list l) r)]
    ['(#f #t) (compare l (list r))]
    ['(#f #f) (compare-list l r)]
  )
)

(define (compare-list l r)
  (if (or (empty? l) (empty? r))
    (- (length l) (length r))
    (let ([res (compare (car l) (car r))])
      (if (= 0 res) (compare-list (cdr l) (cdr r)) res)
    )
  )
)

(define pairs (map parse-pair (split-by-separator (file->lines "13.input") empty-string?)))
;(println pairs)

(define good-indexes (map compare-pairs pairs (build-list (length pairs) add1)))
(println (apply + good-indexes))

(define packets (append (list '((2)) '((6))) (apply append pairs)))
(define sorted (sort packets (lambda (l r) (< (compare l r) 0))))
;(println sorted)
(define i1 (index-of sorted '((2))))
(define i2 (index-of sorted '((6))))
(define decoder-key (* (+ 1 i1) (+ 1 i2)))
(displayln decoder-key)

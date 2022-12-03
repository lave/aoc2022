#lang racket

(define (split-middle s)
  (define m (quotient (string-length s) 2))
  (list (substring s 0 m) (substring s m))
)

(define (split-by lst n)
  (if (not (empty? lst)) (cons (take lst n) (split-by (drop lst n) n)) '())
)

(define (weight item)
  (define c (char->integer item))
  (if (< c 97) (- c 38) (- c 96))
)

(define (find-dup l r)
  (findf (lambda (c) (string-contains? l (list->string (list c)))) (string->list r))
)

(define (find-badge l m r)
  (findf (lambda (c) (and (string-contains? l (list->string (list c))) (string-contains? m (list->string (list c))))) (string->list r))
)


(define lines (file->lines "03.input"))

(define rucksacks (map split-middle lines))
(define dups (map (curry apply find-dup) rucksacks))
(println (apply + (map weight dups)))

(define groups (split-by lines 3))
(define badges (map (curry apply find-badge) groups))
(println (apply + (map weight badges)))

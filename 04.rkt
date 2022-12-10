#lang racket

(define (make-pair s)
  (match-define (list assignment1 assignment2) (string-split s ","))
  (list (make-assignment assignment1) (make-assignment assignment2))
)

(define (make-assignment s)
  (map string->number (string-split s "-"))
)

(define (full-overlap? a1 a2)
  (match-define (list b1 e1) a1)
  (match-define (list b2 e2) a2)
  (or (= b1 b2) (if (< b1 b2) (>= e1 e2) (<= e1 e2)))
)

(define (overlap? a1 a2)
  (match-define (list b1 e1) a1)
  (match-define (list b2 e2) a2)
  (or (and (>= e1 b2) (<= b1 e2)) (and (>= e2 b1) (<= b2 e1)))
)

(define pairs (map make-pair (file->lines "04.input")))

(define full-overlaps (filter (curry apply full-overlap?) pairs))
(displayln (length full-overlaps))

(define overlaps (filter (curry apply overlap?) pairs))
(displayln (length overlaps))

#lang racket

(require "common.rkt" "matrix.rkt")

(define (tree-height c)
  (- (char->integer c) (char->integer #\0))
)

(define at-left?       (compose zero? car))
(define (at-right? w)  (compose (curry = (- w 1)) car))
(define at-top?        (compose zero? cadr))
(define (at-bottom? h) (compose (curry = (- h 1)) cadr))

(define go-left  (curry map-1st sub1))
(define go-right (curry map-1st add1))
(define go-up    (curry map-2nd sub1))
(define go-down  (curry map-2nd add1))

(define (is-visible? forest x y e)
  (define-values (w h) (matrix-dims forest))
  (define crd (list x y))
  (or
    (is-visible_ forest  at-left?      go-left  crd e)
    (is-visible_ forest (at-right? w)  go-right crd e)
    (is-visible_ forest  at-top?       go-up    crd e)
    (is-visible_ forest (at-bottom? h) go-down  crd e)
  )
)

(define (is-visible_ forest at-border? step crd e)
  (or
    (at-border? crd)
    (let ([crd_ (step crd)])
      (and
        (> e (matrix-ref2 forest crd_))
        (is-visible_ forest at-border? step crd_ e)
      )
    )
  )
)

(define (scenic-score forest x y e)
  (define-values (w h) (matrix-dims forest))
  (define crd (list x y))
  (*
    (scenic-score_ forest  at-left?      go-left  crd e 0)
    (scenic-score_ forest (at-right? w)  go-right crd e 0)
    (scenic-score_ forest  at-top?       go-up    crd e 0)
    (scenic-score_ forest (at-bottom? h) go-down  crd e 0)
  )
)

(define (scenic-score_ forest at-border? step crd e score)
  (if (at-border? crd)
    score
    (let ([crd_ (step crd)])
      (if (> e (matrix-ref2 forest crd_))
        (scenic-score_ forest at-border? step crd_ e (add1 score))
        (add1 score)
      )
    )
  )
)


(define forest (lists->matrix (map (lambda (l) (map tree-height (string->list l))) (file->lines "08.input"))))
;(println forest)

(define is-visible (matrix-map (curry is-visible? forest) forest))
;(println is-visible)
(define visible-count (count identity (matrix->list is-visible)))
(println visible-count)

(define scenic-scores (matrix-map (curry scenic-score forest) forest))
;(println scenic-scores)
(define best-scenic-score (apply max (matrix->list scenic-scores)))
(println best-scenic-score)


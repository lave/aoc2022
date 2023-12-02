#lang racket

(require "common.rkt")

(define (move e v)
  ;(println (list e v))
  (define n (vector-length v))
  (define j (vector-member e v))
  (define d (cdr e))
  (define j__ (modulo (+ j (cdr e)) (- n 1)))
  (define j_ (if (= 0 j__) (- n 1) j__))
  ;(define-values (l r) (split-at v j))
  (if (= j j_)
    v
    (if (< j j_) ; move right
      (begin
        ;(displayln (list "1  " (vector-map cdr v)))
        (vector-copy! v j v (+ j 1) (+ j_ 1))
        ;(displayln (list "1  " (vector-map cdr v)))
        (vector-set! v j_ e)
        ;(displayln (list "1  " (vector-map cdr v)))
        v
      )
      (begin
        ;(displayln (list "2  " (vector-map cdr v)))
        (vector-copy! v (+ j_ 1) v j_ j)
        ;(displayln (list "2  " (vector-map cdr v)))
        (vector-set! v j_ e)
        ;(displayln (list "2  " (vector-map cdr v)))
        v
      )
    )
  )
)


(define nums (map string->number (file->lines "20.input")))
(define n (length nums))
;(println nums)
(define nums_ (map cons (range n) nums))
(define v (list->vector nums_))
;(println v)
(define i0 (index-where nums_ (lambda (e) (= 0 (cdr e)))))
(define e0 (list-ref nums_ i0))
;(println e0)

;(define res (foldl (lambda (e v) (move e v)) v nums_))
;(println res)
;(define i0_ (vector-member e0 res))
;(println i0_)
;(define res_ (map (lambda (i) (cdr (vector-ref res (modulo (+ i i0_) (vector-length res))))) '(1000 2000 3000)))
;(println res)
;(displayln (apply + res_))

(define nums2 (map cons (range n) (map (lambda (e) (* e 811589153)) nums)))
(define v2 (list->vector nums2))
(println v2)
(define i2 (index-where nums2 (lambda (e) (= 0 (cdr e)))))
(define e2 (list-ref nums2 i2))
(println e2)

(define res2 (foldl (lambda (k v_) (foldl (lambda (e v_) (move e v_)) v_ nums2)) v2 (range 10)))
;(println res2)
(define i2_ (vector-member e2 res2))
;(println i2_)
(define res2_ (map (lambda (i) (cdr (vector-ref res2 (modulo (+ i i2_) (vector-length res2))))) '(1000 2000 3000)))
;(println res2_)
(displayln (apply + res2_))

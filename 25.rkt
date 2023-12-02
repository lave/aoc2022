#lang racket

(require "common.rkt")

(define (parse-snafu s)
  (define digits (map (lambda (c) (case c [(#\=) -2] [(#\-) -1] [(#\0) 0] [(#\1) 1] [(#\2) 2])) (string->list s)))
  (foldl (lambda (d r) (+ d (* 5 r))) 0 digits)
)

(define (num->snafu n)
  (num->snafu_ n '())
)

(define (num->snafu_ n digits)
  (if (= 0 n)
    digits
    (let-values ([(d r) (quotient/remainder (+ n 2) 5)])
      (num->snafu_ d (cons (- r 2) digits))
    )
  )
)

(define (print-snafu digits)
  (list->string (map (lambda (d) (case d [(-2) #\=] [(-1) #\-] [(0) #\0] [(1) #\1] [(2) #\2])) digits))
)

(define nums (map parse-snafu (file->lines "25.input")))
;(println nums)

(define s (apply + nums))

;(define nums_ (map num->snafu nums))
;(println nums_)

(define res (num->snafu s))
(println (print-snafu res))

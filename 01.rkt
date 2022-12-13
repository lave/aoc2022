#lang racket

(require "common.rkt")


(define lines (file->lines "01.input"))
(define numbers (map string->number lines))
(define inventories (split-by-separator numbers not))
;(println inventories)

(define weights (sort (map (curry apply +) inventories) >))
(displayln (car weights))

(define weights-top3 (take weights 3))
(println (apply + weights-top3))

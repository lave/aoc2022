#lang racket

(define (find-start-of stream len index)
  (let* ([prefix (take stream len)] [prefix-unique (remove-duplicates prefix)])
    (if (= (length prefix) (length prefix-unique))
      (+ index len)
      (find-start-of (cdr stream) len (+ index 1))
    )
  )
)


(define stream (string->list (car (file->lines "06.input"))))
;(println stream)

(define start-of-packet (find-start-of stream 4 0))
(displayln start-of-packet)

(define start-of-message (find-start-of stream 14 0))
(displayln start-of-message)

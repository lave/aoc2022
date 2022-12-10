#lang racket

(define (weight shape)
  (case shape
    [("A" "X") 0]
    [("B" "Y") 1]
    [("C" "Z") 2])
)

(define (score opp you)
  (+
    (+ 1 you)
    (if (= opp you) 3 (if (= you (modulo (+ 1 opp) 3)) 6 0))
  )
)

(define (score1 opp you)
  (score (weight opp) (weight you))
)

(define (score2 opp outcome)
  (define w (weight opp))
  (define you (case outcome
    [("X") (modulo (- w 1) 3)]
    [("Y") w]
    [("Z") (modulo (+ w 1) 3)]))
  (score w you)
)


(define rounds (map string-split (file->lines "02.input")))
;(println rounds)

(define total1 (foldl + 0 (map (curry apply score1) rounds)))
(displayln total1)

(define total2 (foldl + 0 (map (curry apply score2) rounds)))
(displayln total2)

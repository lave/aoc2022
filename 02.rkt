#lang racket

(define (weight shape)
  (case shape [("A" "X") 0] [("B" "Y") 1] [("C" "Z") 2])
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

(define (find-shape outcome w)
  (case outcome [("X") (modulo (- w 1) 3)] [("Y") w] [("Z") (modulo (+ w 1) 3)]))

(define (score2 opp outcome)
  (define w (weight opp))
  (score w (find-shape outcome w))
)


(define rounds (map string-split (file->lines "02.input")))

(define total1 (foldl + 0 (map (curry apply score1) rounds)))
(println total1)

(define total2 (foldl + 0 (map (curry apply score2) rounds)))
(println total2)

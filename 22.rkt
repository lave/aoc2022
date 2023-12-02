#lang racket

(require "common.rkt" "matrix.rkt")

(define (parse-board lines)
  (define w (apply max (map string-length lines)))
  (define m (map (lambda (r) (map (lambda (c) (case c [(#\space) 0] [(#\.) 1] [(#\#) 2])) (string->list r))) lines))
  (define m_ (map (lambda (r) (if (< (length r) w) (append r (build-list (- w (length r)) (const 0))) r)) m))
  (lists->matrix m_)
)
(define (parse-moves s)
  (define s_ (string-replace s "L" " L "))
  (define s__ (string-replace s_ "R" " R "))
  (define ts (string-split s__))
  (define ts_ (map (lambda (t) (case t [("L") 'left] [("R") 'right] [else (string->number t)])) ts))
  ts_
)


(define (make-move board pos move)
  (println (list pos move))
  (define-values (x y d) (apply values pos))
  (define p_ (case move
    ['left (list x y (modulo (- d 1) 4))]
    ['right (list x y (modulo (+ d 1) 4))]
    [else (move-fwd board pos move)]
  ))
  p_
)

(define (move-fwd board pos n)
  (if (= 0 n)
    pos
    (let* ([pos_ (next-pos2 board pos)])
      (if (= 2 (matrix-ref board (car pos_) (cadr pos_)))
        pos
        (move-fwd board pos_ (- n 1))
      )
    )
  )
)

(define (next-pos1 board pos)
  (define-values (x y d) (apply values pos))
  (define-values (w h) (matrix-dims board))
  (define pos_ (case d
                 [(0) (list (modulo (+ x 1) w) y d)]
                 [(1) (list x (modulo (+ y 1) h) d)]
                 [(2) (list (modulo (- x 1) w) y d)]
                 [(3) (list x (modulo (- y 1) h) d)]))
  (if (= 0 (matrix-ref board (car pos_) (cadr pos_))) (next-pos1 board pos_) pos_)
)

(define (next-pos2 board pos)
  (define-values (x y d) (apply values pos))
  (define-values (w h) (matrix-dims board))
  (define pos_ (case d
                 [(0) (list (modulo (+ x 1) w) y d)]
                 [(1) (list x (modulo (+ y 1) h) d)]
                 [(2) (list (modulo (- x 1) w) y d)]
                 [(3) (list x (modulo (- y 1) h) d)]))
  (if (= 0 (matrix-ref board (car pos_) (cadr pos_)))
    (let* (
           [xs (quotient x 50)]
           [ys (quotient y 50)]
           [sn (get-square xs ys)])
      (next-wrap pos sn)
    )
    pos_
  )
)

(define (get-square xs ys)
  (define sn (case (cons xs ys)
    [((1 . 0)) 0]
    [((2 . 0)) 1]
    [((1 . 1)) 2]
    [((0 . 2)) 3]
    [((1 . 2)) 4]
    [((0 . 3)) 5]
  ))
  (println (list xs ys sn))
  sn
)

(define (next-wrap pos sn)
  (define-values (x y d) (apply values pos))
  (define p_ (case (cons sn d)
    [((0 . 0)) (next-pos1 #f pos)]
    [((0 . 1)) (next-pos1 #f pos)]
    [((0 . 2)) (list 0 (+ 100 (- 49 y)) 0)] ; 0 -> 3
    [((0 . 3)) (list 0 (+ 150 (- x 50)) 0)] ; 0 -> 5

    [((1 . 0)) (list 99 (+ 100 (- 49 y)) 2)] ; 1 -> 4
    [((1 . 1)) (list 99 (+ 50 (- x 100)) 2)] ; 1 -> 2
    [((1 . 2)) (next-pos1 #f pos)]
    [((1 . 3)) (list (- x 100) 199 3)]       ; 1 -> 5

    [((2 . 0)) (list (+ 100 (- y 50)) 49 3)] ; 2 -> 1
    [((2 . 1)) (next-pos1 #f pos)]
    [((2 . 2)) (list (- y 50) 100 1)] ; 2 -> 3
    [((2 . 3)) (next-pos1 #f pos)]

    [((3 . 0)) (next-pos1 #f pos)]
    [((3 . 1)) (next-pos1 #f pos)]
    [((3 . 2)) (list 50 (- 149 y) 0)] ; 3 -> 0
    [((3 . 3)) (list 50 (+ 50 x) 0)] ; 3 -> 2

    [((4 . 0)) (list 149 (- 149 y) 2)] ; 4 -> 1
    [((4 . 1)) (list 49 (+ 150 (- x 50)) 2)] ; 4 -> 5
    [((4 . 2)) (next-pos1 #f pos)]
    [((4 . 3)) (next-pos1 #f pos)]

    [((5 . 0)) (list (+ 50 (- y 150)) 149 3)] ; 5 -> 4
    [((5 . 1)) (list (+ 100 x) 0 1)] ; 5 -> 1
    [((5 . 2)) (list (+ 50 (- y 150)) 0 1)] ; 5 -> 0
    [((5 . 3)) (next-pos1 #f pos)]
  ))
  (println (list sn ": " pos " wraps to " p_))
  p_
)


(define-values (lines-board line-moves) (splitf-at (file->lines "22.input") non-empty-string?))
(define board (parse-board lines-board))
(println board)
(define moves (parse-moves (cadr line-moves)))
(println moves)

(define x0 (vector-member 1 (vector-ref board 0)))
(define p0 (list x0 0 0))
(println x0)

(define p1 (foldl (lambda (move pos) (make-move board pos move)) p0 moves))
(println p1)
(define res1 (+ (* 1000 (+ 1 (cadr p1))) (* 4 (+ 1 (car p1))) (caddr p1)))
(println res1)

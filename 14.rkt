#lang racket

(require "common.rkt" "matrix.rkt")

(define (parse-lines s)
  (define tokens (string-split s " -> "))
  (define points (map (lambda (t) (map string->number (string-split t ","))) tokens))
  (define lines (map list (reverse (cdr (reverse points))) (cdr points)))
  lines
)

(define (move-point dx dy point)
  (list (- (car point) dx) (- (cadr point) dy))
)

(define (print-cave cave)
  (vector-map (lambda (row) (displayln (list->string (vector->list (vector-map (lambda (e) (case e [(0) #\.] [(1) #\#] [(2) #\o])) row))))) cave)
  (void)
)

(define (draw-lines lines matrix)
  (foldl draw-line matrix lines)
)

(define (draw-line line matrix)
  (define x1 (caar line))
  (define y1 (cadar line))
  (define x2 (caadr line))
  (define y2 (cadadr line))
  (define points (if (= x1 x2)
                   (map (lambda (y) (list x1 y)) (range (min y1 y2) (+ 1 (max y1 y2))))
                   (map (lambda (x) (list x y1)) (range (min x1 x2) (+ 1 (max x1 x2))))
                   ))
  (foldl (lambda (point m) (matrix-set2 m point 1)) matrix points)
)

(define (trace-sand cave p n)
  (define-values (x y) (apply values p))
  (define r (trace-sand_ cave x y))
  (if (and (cadr r) (< n 100000))
    (trace-sand (car r) p (+ 1 n))
    (list (car r) n)
  )
)

(define (trace-sand_ cave x y)
  (define h (vector-length cave))
  (if (= y (- h 1))
    (list cave #f)    ;   fall through
    (if (= 0 (matrix-ref cave x (+ y 1)))
      (trace-sand_ cave x (+ y 1))   ;   fall down
      (if (= 0 (matrix-ref cave (- x 1) (+ y 1)))
        (trace-sand_ cave (- x 1) (+ y 1))   ;   fall down-left
        (if (= 0 (matrix-ref cave (+ x 1) (+ y 1)))
          (trace-sand_ cave (+ x 1) (+ y 1))   ;   fall down-right
          (if (= 0 y)
            (list cave #f)    ;   end
            (list (matrix-set cave x y 2) #t)    ;   settle
          )))))
)


(define lines (apply append (map parse-lines (file->lines "14.input"))))
;(println lines)
(define points (remove-duplicates (apply append lines)))
;(println points)
(define x-min (apply min (map car points)))
(define x-max (apply max (map car points)))
(define y-min (apply min (map cadr points)))
(define y-max (apply max (map cadr points)))
;(println (list x-min x-max y-min y-max))

(define w (+ 3 (- x-max x-min)))
(define x0 (- x-min 1))
(define dy (quotient (- w 1) 2))
;(define y0 (- y-min dy))
(define y0 0)
(define h (+ 2 (- y-max y0)))
(define sand-gen (list (- 500 x0) 0))
;(println (list w h x0 y0))

(define lines_ (map (lambda (line) (map (lambda (point) (move-point x0 y0 point)) line)) lines))
;(println lines_)

(define cave (matrix-set2 (draw-lines lines_ (create-matrix w h 0)) sand-gen 2))
;(print-cave cave)

(define res (trace-sand cave sand-gen 0))
;(print-cave (car res))
(displayln (cadr res))

(define h2 (+ 1 h))
(define w2 (* 2 h2))
(define x02 (- 500 h2))
(define lines2 (cons (list (list 0 h) (list (- w2 1) h)) (map (lambda (line) (map (lambda (point) (move-point x02 y0 point)) line)) lines)))

(define sand-gen2 (list (- 500 x02) 0))
(define cave2 (matrix-set2 (draw-lines lines2 (create-matrix w2 h2 0)) sand-gen2 2))
;(print-cave cave2)

(define res2 (trace-sand cave2 sand-gen2 0))
(print-cave (car res2))
(displayln (cadr res2))

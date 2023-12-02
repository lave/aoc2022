#lang racket

(require "matrix.rkt")

(define (parse-map lines)
  (define map_ (map string->list lines))
  (define h (length map_))
  (define w (length (car map_)))
  (define s (find-point #\S map_))
  (define e (find-point #\E map_))
  (define map__ (list->vector (map parse-row map_)))
  (list w h s e map__)
)

(define (find-point p lines)
  (define y (index-where lines (lambda (row) (member p row))))
  (define x (index-of (list-ref lines y) p))
  (list x y)
)
  
(define (parse-row row)
  (list->vector (map (lambda (p) (if (equal? p #\S) 0 (if (equal? p #\E) 25 (- (char->integer p) 97)))) row))
)

(define (set-point m coord value)
  ;(define row (vector-ref m (cadr coord)))
  ;(define row_ (vector-set! row (car coord) value))
  ;(vector-set! m (cadr coord) row_)
  ;(println (list coord value))
  (vector-set! (vector-ref m (cadr coord)) (car coord) value)
  m
)

(define (get-point m x y)
  (vector-ref (vector-ref m y) x)
)

(define (get-point_ m coord)
  (vector-ref (vector-ref m (cadr coord)) (car coord))
)

(define (find-route map_)
  (define-values (w h s e m) (apply values map_))
  (define l (build-vector h (lambda (i) (build-vector w (const -1)))))
  (define l0 (set-point l e 0))
  ;(println l0)
  (find-route_ w h s e m l0)
)

(define (while condition body)
  (when (condition)
    (body)
    (while condition body)))

(define (find-route_ w h s e m l)
  (define r (vector 0))
  (for ([y (build-list h identity)])
    (for ([x (build-list w identity)])
      (if (update-point x y w h m l)
        ; continue searching
        (vector-set! r 0 -1)
        #f
      )
    )
  )
  ;(println l)

  (define ls (get-point_ l s))
  ;(define ls (vector-ref r 0))
  (if (= -1 ls)
    (find-route_ w h s e m l)
    ;(find-min-a w h m l))
    ls
    )
)

(define (find-min-a w h m l)
  ;(println l)
  (define r (vector 500))
  (for ([y (build-list h identity)])
    (for ([x (build-list w identity)])
      (if (and (= 0 (get-point m x y)) (> (get-point l x y) -1) (< (get-point l x y) (vector-ref r 0)))
        (vector-set! r 0 (get-point l x y))
        #f
      )
    )
  )
  (vector-ref r 0)
)

(define (update-point x y w h m l)
  (define alt (get-point m x y))
  (if (= (get-point l x y) -1)
    (let* ([up    (if (> y 0)       (try-point alt x (- y 1) m l) -1)]
           [down  (if (< y (- h 1)) (try-point alt x (+ y 1) m l) -1)]
           [left  (if (> x 0)       (try-point alt (- x 1) y m l) -1)]
           [right (if (< x (- w 1)) (try-point alt (+ x 1) y m l) -1)]
           [all (sort (filter (lambda (v) (>= v 0)) (list up down left right)) <)])
      (begin
        ;(println (list x y all))
        (if (not (empty? all)) (begin (set-point l (list x y) (car all)) #t) #f)
      )
    )
    #f
  )
)

(define (try-point alt x y m l)
  (define alt_ (get-point m x y))
  (define l_ (get-point l x y))
  (if (and (>= l_ 0) (>= (+ alt 1) alt_)) (+ 1 l_) -1)
)



(define map_ (parse-map (file->lines "12.input")))
;(println map_)

(define l (find-route map_))
(displayln l)

(define l2 (find-route map_))
(displayln l2)

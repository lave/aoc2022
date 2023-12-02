#lang racket

(require "common.rkt" "matrix.rkt")

(define (parse-winds_ lines)
  (define winds (apply append (map (lambda (r y)
    (filter identity (map (lambda (c x) (if (or (equal? c #\.) (equal? c #\#)) #f (list c x y))) (string->list r)
                          (range (string-length r)))))
    lines (range (length lines)))))
  (define winds-u (map cdr (filter (lambda (w) (equal? #\^ (car w))) winds)))
  (define winds-d (map cdr (filter (lambda (w) (equal? #\v (car w))) winds)))
  (define winds-l (map cdr (filter (lambda (w) (equal? #\< (car w))) winds)))
  (define winds-r (map cdr (filter (lambda (w) (equal? #\> (car w))) winds)))
  (list (- (string-length (car lines)) 1) (- (length lines) 1) winds-u winds-d winds-l winds-r)
)

(define (trim-both l)
  (reverse (cdr (reverse (cdr l))))
)

(define (winds-m lines c_)
  (lists->matrix (map (lambda (r)
    (map (lambda (c) (if (equal? c c_) 1 0)) (trim-both (string->list r))))
    (trim-both lines)))
)

(define (parse-winds lines)
  (define mu (winds-m lines #\^))
  (define md (winds-m lines #\v))
  (define ml (winds-m lines #\<))
  (define mr (winds-m lines #\>))
  (define-values (w h) (matrix-dims mu))
  (list w h mu md ml mr)
)

(define (free winds n x y)
  (define-values (w h mu md ml mr) (apply values winds))
  ;(print (list "free?" n x y))
  (define r (and
    (= 0 (matrix-ref mu x (modulo (+ y n) h)))
    (= 0 (matrix-ref md x (modulo (- y n) h)))
    (= 0 (matrix-ref ml (modulo (+ x n) w) y))
    (= 0 (matrix-ref mr (modulo (- x n) w) y))
  ))
  ;(println (list " = " r))
  r
)

(define (find-path winds n x y visited period m k)
  ;(println (list n x y (modulo n period) k))
  (define w- (- (car winds) 1))
  (define h- (- (cadr winds) 1))
  (define n_ (+ n 1))
  (if (or
        (set-member? visited (list (modulo n period) x y))
        (> n (* m (+ x y 1))))
    #f
    (if (and (= x w-) (= y h-))
      (begin
        (println n_)
        n_
      )
      (let* ([visited_ (set-add visited (list (modulo n period) x y))]
             [x+ (+ x 1)]
             [x- (- x 1)]
             [y+ (+ y 1)]
             [y- (- y 1)]
             [moves (filter identity (list
                (if (and (>= y 0) (< x w-)       (free winds n_ x+ y)) (cons x+ y) #f)
                (if (and (< y h-)                (free winds n_ x y+)) (cons x y+) #f)
                (if (and (>= y 0) (> x 0)        (free winds n_ x- y)) (cons x- y) #f)
                (if (and (> y 0)                 (free winds n_ x y-)) (cons x y-) #f)
                (if (if (= y -1) (< n w-) (free winds n_ x y)) (cons x y) #f)))]
             [k_ (* k (length moves))]
             )
             (begin
              ;(println moves)
              (let* ([res (filter identity (map (lambda (move) (find-path winds n_ (car move) (cdr move) visited_ period m k_)) moves))])
                (if (empty? res) #f (apply min res))
              )
             )
      )
    )
  )
)

(define (next-poses winds n pos)
  (define x (car pos))
  (define y (cdr pos))
  (define w- (- (car winds) 1))
  (define h- (- (cadr winds) 1))
  (define n_ (+ n 1))
      (let* ([x+ (+ x 1)]
             [x- (- x 1)]
             [y+ (+ y 1)]
             [y- (- y 1)]
             [moves (filter identity (list
                (if (and (>= y 0) (<= y h-) (< x w-)       (free winds n_ x+ y)) (cons x+ y) #f)
                (if (and (< y h-)                (free winds n_ x y+)) (cons x y+) #f)
                (if (and (>= y 0) (<= y h-) (> x 0)        (free winds n_ x- y)) (cons x- y) #f)
                (if (and (> y 0)                 (free winds n_ x y-)) (cons x y-) #f)
                (if (or (= y -1) (= y (+ h- 1)) (free winds n_ x y)) (cons x y) #f)))]
             )
          (filter identity moves)
             )
)

(define (find-path-wide winds n poss back?)
  ;(println (list n poss))
  (define w- (- (car winds) 1))
  (define h- (- (cadr winds) 1))
  (define n_ (+ n 1))
  (define poss_ (remove-duplicates (apply append (map (lambda (p) (next-poses winds n p)) poss))))
  (if (member (if back? (cons 0 0 )(cons w- h-)) poss_)
    (+ n 2)
    (find-path-wide winds n_ poss_ back?)
  )
)

(define (print-winds winds i)
  (define w (car winds))
  (define h (cadr winds))
  (displayln (list "---" i))
  (map (lambda (y)
    (displayln (list->string (map (lambda (x) (if (free winds i x y) #\. #\x)) (range w))))) (range h))
  (void)
)


;(define winds (parse-winds (file->lines "24_.input")))
;(define path (find-path winds 0 0 -1 (set) 12))

(define winds (parse-winds (file->lines "24.input")))
(define w (car winds))
(define h (cadr winds))

;(for ([i (range 302)]) (print-winds winds i))

;(define path (find-path winds 0 0 -1 (set) 300 1.75 1))
(define path (find-path-wide winds 0 (list (cons 0 -1)) #f))
(println path)
(define path1 (find-path-wide winds path (list (cons (- w 1) h)) #t))
(println path1)
(define path2 (find-path-wide winds path1 (list (cons 0 -1)) #f))
(println path2)

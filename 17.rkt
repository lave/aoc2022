#lang racket

(require "common.rkt" "matrix.rkt")

(define rocks '(
                ((2 . 0) (3 . 0) (4 . 0) (5 . 0))
                ((3 . 0) (2 . 1) (3 . 1) (4 . 1) (3 . 2))
                ((2 . 0) (3 . 0) (4 . 0) (4 . 1) (4 . 2))
                ((2 . 0) (2 . 1) (2 . 2) (2 . 3))
                ((2 . 0) (3 . 0) (2 . 1) (3 . 1))
               ))
 

(define (set-y rock y)
  (map (lambda (p) (cons (car p) (+ y (cdr p)))) rock)
)

(define (is-point-free m p)
  (define x (car p))
  (define y (cdr p))
  (and (<= 0 x) (> 7 x) (<= 0 y) (= 0 (matrix-ref2_ m p)))
)

(define (and_ l r) (and l r))

(define (move-rock-wind m rock dx)
  (define rock_ (map (lambda (p) (cons (+ dx (car p)) (cdr p))) rock))
  (define can-move (foldl and_ #t (map (lambda (p) (is-point-free m p)) rock_)))
  (if can-move rock_ rock)
)

(define (move-rock-down m rock)
  (define rock_ (map (lambda (p) (cons (car p) (- (cdr p) 1))) rock))
  (define can-move (foldl and_ #t (map (lambda (p) (is-point-free m p)) rock_)))
  (if can-move (list rock_ #t) (list rock #f))
)

(define (settle-rock m rock)
  (define m_ (foldl (lambda (p m) (matrix-set2_ m p 1)) m rock))
  m_
)

(define (fall-rock i state rock)
  (define-values (m y wind wi) (apply values state))
  (define move (vector-ref wind wi))
  (define wi_ (modulo (+ wi 1) (vector-length wind)))
  (if (= 0 wi_)
    (begin
      (print-chamber m rock)
      (println (list i wi_ y))
    )
    (void)
    )
  (define rock_ (move-rock-wind m rock move)) 
  ;(print-chamber m rock_)
  (define-values (rock__ moved-down) (apply values (move-rock-down m rock_)))
  ;(print-chamber m rock__)
  (if moved-down
    (fall-rock i (list m y wind wi_) rock__)
    (let* ([m_ (settle-rock m rock__)]
           [y_ (apply max (cons y (map (compose add1 cdr) rock__)))])
      (list m_ y_ wind wi_)
    )
  )
)

(define (add-rock i state)
  (define-values (m y wind wi) (apply values state))
  (define rock (set-y (list-ref rocks (modulo i 5)) (+ y 3)))
  ;(print-chamber m rock)
  (define state_ (fall-rock i state rock))
  ;(print-chamber (car state_) '())
  state_
)


(define (print-chamber m rock)
  (define m_ (if (empty? rock) m (settle-rock (matrix-copy m) rock)))
  (map
    (lambda (row) (displayln (list->string (vector->list (vector-map (lambda (e) (case e [(0) #\.] [(1) #\#])) row)))))
    (take (filter (lambda (r) (< 0 (apply + (vector->list r)))) (reverse (vector->list m_))) 10))
  (displayln "-------")
  (displayln "")
  (void)
)


(define wind (list->vector (map (lambda (c) (if (equal? c #\<) -1 1)) (apply append (map string->list (file->lines "17.input"))))))
;(println wind)
(println (vector-length wind))

(define m (create-matrix 7 20000 0))
(define res (foldl add-rock (list m 0 wind 0) (range (+ 2755 1745))))
(displayln (cadr res))

; rocks period: 1745
; height period: 2783

; 573065902.578796561604585
; 573065901
; 999999997245
; h = 1594842402483 _ 

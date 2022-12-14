#lang racket

(define (parse-move line)
  (define ts (string-split line))
  (define dir (string-ref (car ts) 0))
  (define n (string->number (cadr ts)))
  (build-list n (const dir))
)

(define (move-knot current moved)
  ; coordinates of "head" knot - know which was moved last
  (define-values (xh yh) (apply values (car moved)))
  ; coordinates of "tail" know - know which moves next following the "head" knot
  (define-values (xt yt) (apply values current))

  ; calculate new coordinates for "tail" knot
  (define xt_ (case (- xh xt)
      [(-2) (+ xh 1)]
      [(-1) (if (= 2 (abs (- yh yt))) xh xt)]
      [(0) xt]
      [(1) (if (= 2 (abs (- yh yt))) xh xt)]
      [(2) (- xh 1)]
  ))
  (define yt_ (case (- yh yt)
      [(-2) (+ yh 1)]
      [(-1) (if (= 2 (abs (- xh xt))) yh yt)]
      [(0) yt]
      [(1) (if (= 2 (abs (- xh xt))) yh yt)]
      [(2) (- yh 1)]
  ))

  (cons (list xt_ yt_) moved)
)

(define (move-head coord move)
  (define-values (x y) (apply values coord))
  (define x_ (if (equal? #\L move) (- x 1) (if (equal? #\R move) (+ x 1) x)))
  (define y_ (if (equal? #\D move) (- y 1) (if (equal? #\U move) (+ y 1) y)))
  (list x_ y_)
)

(define (make-move move state)
  (define-values (rope visited) (apply values state))
  (define new-head (move-head (car rope) move))
  (define new-rope (foldl move-knot (list new-head) (cdr rope)))
  (define new-tail (car new-rope))
  (list (reverse new-rope) (cons new-tail visited))
)

(define (find-tail-visited-count rope-length moves)
  (define tail-positions (foldl make-move (list (build-list rope-length (const '(0 0))) '((0 0))) moves))
  (length (remove-duplicates (cadr tail-positions)))
)


(define moves (apply append (map parse-move (file->lines "09.input"))))
;(println moves)

(displayln (find-tail-visited-count 2 moves))

(displayln (find-tail-visited-count 10 moves))

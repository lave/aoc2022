#lang racket

(require "common.rkt" "matrix.rkt")

(define (parse-map lines)
  ;(define m (map (lambda (r) (map (lambda (c) (case c [(#\.) 0] [(#\#) 1])) (string->list r))) lines))
  ;(lists->matrix m)
  (define elves (apply append (map (lambda (r y)
    (filter identity (map (lambda (c x) (if (equal? c #\.) #f (cons x y))) (string->list r) (range (string-length r)))))
    lines (range (length lines)))))
  elves
)

(define (move i elves)
  (define dir0 (modulo i 4))
  (define eset (list->set elves))
  (define proposed (map (lambda (e) (find-proposed eset e dir0)) elves))
  ;(println proposed)
  (define prop-counts (make-hash))
  (foldl (lambda (p h) (begin (hash-update! h p add1 0) h)) prop-counts proposed)
  (define elves_ (map (lambda (e p) (if (= 1 (hash-ref prop-counts p)) p e)) elves proposed))
  (if (equal? elves elves_)
    (println (list "FOUND: " (+ i 1)))
    #f)
  elves_
)

(define (or_ l r) (or l r))

(define (find-proposed eset elf dir0)
  ;(define no-move (cons elf elf))
  (define no-move elf)
  (define x (car elf))
  (define y (cdr elf))
  (define nw (set-member? eset (cons (- x 1) (- y 1))))
  (define n  (set-member? eset (cons x       (- y 1))))
  (define ne (set-member? eset (cons (+ x 1) (- y 1))))
  (define w  (set-member? eset (cons (- x 1) y      )))
  (define e  (set-member? eset (cons (+ x 1) y      )))
  (define sw (set-member? eset (cons (- x 1) (+ y 1))))
  (define s  (set-member? eset (cons x       (+ y 1))))
  (define se (set-member? eset (cons (+ x 1) (+ y 1))))
  (if (or nw n ne w e sw s se)
    (let* ([move-n (if (or nw n ne) #f (cons x (- y 1)))]
           [move-s (if (or sw s se) #f (cons x (+ y 1)))]
           [move-w (if (or nw w sw) #f (cons (- x 1) y))]
           [move-e (if (or ne e se) #f (cons (+ x 1) y))]
           [moves (case dir0
             [(0) (list move-n move-s move-w move-e)]
             [(1) (list move-s move-w move-e move-n)]
             [(2) (list move-w move-e move-n move-s)]
             [(3) (list move-e move-n move-s move-w)])]
           [valid-moves (filter identity moves)]
          )
        ;(if (empty? valid-moves) no-move (cons elf (car valid-moves)))
        (if (empty? valid-moves) no-move (car valid-moves))
    )
    no-move   ; no move, all neighbours are free
  )
)

(define (empty-count elves)
  (define xmin (apply min (map car elves)))
  (define xmax (apply max (map car elves)))
  (define ymin (apply min (map cdr elves)))
  (define ymax (apply max (map cdr elves)))
  (- (* (add1 (- xmax xmin)) (add1 (- ymax ymin))) (length elves))
)

(define elves (parse-map (file->lines "23.input")))
;(println elves)

;(define elves_ (move 0 elves))
(define elves_ (foldl move elves (range 1000)))
;(println elves_)
(println (empty-count elves_))

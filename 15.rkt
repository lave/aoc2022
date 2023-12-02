#lang racket

(require "common.rkt" "matrix.rkt")

(define (parse-sensor s)
  (define tokens (string-split s "="))
  (define xs (string->number (car (string-split (list-ref tokens 1) ","))))
  (define ys (string->number (car (string-split (list-ref tokens 2) ":"))))
  (define xb (string->number (car (string-split (list-ref tokens 3) ","))))
  (define yb (string->number (list-ref tokens 4)))
  (list (list xs ys) (list xb yb) (+ (abs (- xs xb)) (abs (- ys yb))))
)

(define (find-section y sensor)
  (define-values (xs ys) (apply values (car sensor)))
  (define r (caddr sensor))
  (define dy (- r (abs (- y ys))))
  (if (< dy 0)
    '()
    (list (- xs dy) (+ xs dy))
  )
)

(define (merge-section section sections)
  (define-values (s1 e1) (apply values (car sections)))
  (define-values (s2 e2) (apply values section))
  (if (> (- s2 e1) 1)
    (cons section sections)     ; no intersections
    (cons (list s1 (max e1 e2)) (cdr sections))
  )
)

(define (has-hole-in-range xmin xmax sections)
  ;(println sections)
  (if (empty? sections)
    #f
    (findf (lambda (e) (and (< (car e) xmin) (> (cadr e) xmin) (< (cadr e) xmax))) sections)
  )
)

(define (all-sections sensors y xmin xmax)
  (if (= 0 (modulo y 1000)) (println y) '())
  (define sections (filter (lambda (l) (not (empty? l))) (map (curry find-section y) sensors)))
  (define sorted (sort sections (lambda (s1 s2) (< (car s1) (car s2)))))
  (define merged (reverse (foldl merge-section (list (car sorted)) (cdr sorted))))
  (if (has-hole-in-range xmin xmax merged) merged #f)
)

(define sensors (map parse-sensor (file->lines "15.input")))
(println sensors)

;(define sections (filter (lambda (l) (not (empty? l))) (map (curry find-section 2000000) sensors)))
;(println sections)

;(define sorted (sort sections (lambda (s1 s2) (< (car s1) (car s2)))))
;(println sorted)

;(define merged (reverse (foldl merge-section (list (car sorted)) (cdr sorted))))
;(println merged)

;(define count (apply + (map (lambda (l) (- (cadr l) (car l))) merged)))
;(println count)

;(define res (findf (map (lambda (y) (all-sections sensors y)) (range 0 4000001)) identity))
(define res (findf (lambda (y) (all-sections sensors y 0 4000000)) (range 0 4000001)))
(println res)

(define res1 (all-sections sensors res 0 4000000))
(println res1)

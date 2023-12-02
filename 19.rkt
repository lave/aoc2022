#lang racket

(require "common.rkt")

(define (parse-blueprint s)
  (define tokens (string-split s))
  (define n (string->number (string-trim (list-ref tokens 1) ":")))
  (define ore-ore (string->number (list-ref tokens 6)))
  (define clay-ore (string->number (list-ref tokens 12)))
  (define obsidian-ore (string->number (list-ref tokens 18)))
  (define obsidian-clay (string->number (list-ref tokens 21)))
  (define geode-ore (string->number (list-ref tokens 27)))
  (define geode-obsidian (string->number (list-ref tokens 30)))
  (cons n (list
            (list ore-ore 0 0 0)
            (list clay-ore 0 0 0)
            (list obsidian-ore obsidian-clay 0 0)
            (list geode-ore 0 geode-obsidian 0)
            (map (lambda (v) (* 1 v)) (list (max ore-ore clay-ore obsidian-ore geode-ore) obsidian-clay geode-obsidian 0))
            ))
)

(define (and_ l r) (and l r))

(define (has-resources need has type)
  (define r (if (andmap <= need has)
    (cons type need)
    #f
  ))
  ;(println (list "has? " need has ": " r))
  r
)

(define (simulate blueprint time-left robots resources logg)
  ;(println (list time-left robots resources))
  (define-values (r-ore r-clay r-obsidian r-geode) (apply values robots))
  (define-values (ore clay obsidian geode) (apply values resources))
  (define-values (need-ore need-clay need-obsidian need-geode need-max) (apply values (cdr blueprint)))
  (if (= 0 time-left)
    (begin
      #;(if (<= 54 geode) (println (list "DONE: " geode (reverse logg))) (void))
      geode
    )
    (let* (
           [build-nothing (list 0 0 0 0 0)]
           [need (map - resources robots need-max)] ; consider resources at previous move - i.e. allow one move to build
           ; other robots even when we have enough resources
           [build-ore-robot (if (< 0 (car need)) #f (has-resources need-ore resources 1))]
           [build-clay-robot (if (< 0 (cadr need)) #f (has-resources need-clay resources 2))]
           [build-obsidian-robot (if (< 0 (caddr need)) #f (has-resources need-obsidian resources 3))]
           [build-geode-robot (has-resources need-geode resources 4)]
           [moves_ (take-safe (filter identity
             (list build-geode-robot build-obsidian-robot build-clay-robot build-ore-robot build-nothing)) 2)]
           #;[moves_ (if (= 4 (length moves))
             moves
             ;(if (< (length moves) 2)
               (cons build-nothing moves)
               ;(cons build-nothing (take moves 2)))
           )]
           #;[moves_
             (if build-geode-robot
               (list build-geode-robot)
               (filter identity (list build-obsidian-robot build-clay-robot build-ore-robot build-nothing))
               #;(if build-obsidian-robot
                 (list build-obsidian-robot)
                 (filter identity (list build-clay-robot build-ore-robot build-nothing))
               )
               )]
          )
      (begin
       ;(println (list "need: " need "moves: " moves))
       (apply max (map (lambda (what-to-build) (build-robot blueprint time-left robots resources what-to-build logg)) moves_))
       )
    )
  )
)

(define (build-robot blueprint time-left robots resources what-to-build logg)
  ;(println (list "building " what-to-build))
  (define resources_ (map + resources robots))
  (define time-left_ (- time-left 1))
  (define i (car what-to-build))
  (define logg_ (cons (car what-to-build) logg))
  (if (= 0 i)
    (simulate blueprint time-left_ robots resources_ logg_)
    (let* ([resources__ (map - resources_ (cdr what-to-build))]
           [robots_ (list-set robots (- i 1) (+ 1 (list-ref robots (- i 1))))]
          )
        (simulate blueprint time-left_ robots_ resources__ logg_)
    )
  )
)

(define (quality-level blueprint)
  (define geodes (simulate blueprint 24 '(1 0 0 0) '(0 0 0 0) '()))
  (define level (* geodes (car blueprint)))
  (println (list "blueprint #" (car blueprint) ": " level))
  level
)

(define (geodes blueprint)
  (define geodes (simulate blueprint 32 '(1 0 0 0) '(0 0 0 0) '()))
  (println (list "blueprint #" (car blueprint) ": " geodes))
  geodes
)

(define blueprints (map parse-blueprint (file->lines "19.input")))
(println blueprints)

(define levels (map quality-level blueprints))
(displayln (apply + levels))

;(define level (geodes (cadr blueprints)))
;(println level)

(define geodes_ (map geodes (take blueprints 3)))
(displayln (apply * geodes_))

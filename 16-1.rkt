#lang racket

(require "common.rkt" "matrix.rkt")

(define (parse-valve s)
  (define s_ (string-replace s ", " ","))
  (define tokens (string-split s_))
  (define name (list-ref tokens 1))
  (define rate (string->number (string-replace (cadr (string-split (list-ref tokens 4) "=")) ";" "")))
  (define tunnels (string-split (list-ref tokens 9) ","))
  (list name rate tunnels)
)

(define (find valves time-left current visited open-valves open-sum released logg)
  (println (list (- 31 time-left) current open-valves open-sum released))
  (displayln (reverse logg))
  (if (= 0 time-left)
    (begin
      (println (list "DONE" released (reverse logg)))
      (list released logg)
    )
    (let* ([cur-weight (* open-sum (- time-left 1))]
           [valve (hash-ref valves current #f)]
           [rate (car valve)]
           [tunnels (cadr valve)]
           [time-left_ (- time-left 1)]
           [released_ (+ released open-sum)]
           ; open the valve (if not open yet)
           [r0 (if (or (= 0 rate) (member current open-valves))
             (list (+ released_ cur-weight) (cons "idle" logg))
             (find valves time-left_ current visited (cons current open-valves) (+ rate open-sum) released_ (cons "open" logg)))]
           ; go to another valve
           [rs (map (lambda (v)
             (let ([cur-weight (* open-sum (- time-left 1))]
                   [old-weight (hash-ref visited v -1)])
               (if (> cur-weight old-weight)
                 (find valves time-left_ v (hash-set visited v cur-weight) open-valves open-sum released_ (cons v logg))
                 (list (+ released_ cur-weight) (cons "idle" logg))
               )
             ))
             tunnels)]
           )
      (argmax car (cons r0 rs))
    )
  )
)


(define valves (map parse-valve (file->lines "16_.input")))
(define valves_ (make-immutable-hash valves))
;(println valves_)

(define res (find valves_ 30 "AA" (hash-set (hash) "AA" 0) '() 0 0 '()))
(displayln res)

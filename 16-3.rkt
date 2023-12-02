#lang racket

(require "common.rkt")

(define (parse-valve s)
  (define s_ (string-replace s ", " ","))
  (define tokens (string-split s_))
  (define name (list-ref tokens 1))
  (define rate (string->number (string-replace (cadr (string-split (list-ref tokens 4) "=")) ";" "")))
  (define tunnels (string-split (list-ref tokens 9) ","))
  (list name rate (map (lambda (t) (list t 1)) tunnels))
)

(define (find-paths valves)
  (define paths (hash-map valves
    (lambda (k v) (map (lambda (p) (list (list k (car p)) (cdr p))) (hash->list (find-paths_ valves k 0 (hash) '()))))
    ))
  (make-immutable-hash (apply append paths))
)

(define (find-paths_ valves valve path paths visited)
  (define paths_ (hash-update paths valve (lambda (v0) (min v0 path)) path))
  (if (member valve visited)
    paths_
    (let* (
      [edges (cadr (hash-ref valves valve #f))]
      [visited_ (cons valve visited)]
      [paths__ (foldl (lambda (edge pths) (find-paths_ valves (car edge) (+ path (cadr edge)) pths visited_)) paths_ edges)])
      paths__
    )
  )
)

(define (find valves time-left current visited open-valves open-sum released logg)
  ;(println (list (- 31 time-left) current open-valves open-sum released))
  ;(displayln (reverse logg))
  (if (= 0 time-left)
    (begin
      ;(println (list "DONE" released (reverse logg)))
      (list released (reverse logg))
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
             (find valves time-left_ current '() (cons current open-valves) (+ rate open-sum) released_ (cons "open" logg)))]
           ; go to another valve
           [rs (map (lambda (vv)
             (let* ([v (car vv)] [w (cadr vv)])
               (if (or (>= w time-left) (member v visited))
                 (list (+ released_ cur-weight) (cons "idle" logg))
                 (find valves (- time-left w) v (cons v visited) open-valves open-sum (+ released (* w open-sum)) (cons v logg))
               )
             ))
             tunnels)]
           )
      (argmax car (cons r0 rs))
    )
  )
)

(define (simplify-valves valves)
  (define v0 (findf (lambda (v) (and (= 0 (cadr v)) (= 2 (length (caddr v))))) valves))
  (if (not v0)
    valves
    (let* (
       [vs (caddr v0)]
       [v1 (car vs)]
       [v2 (cadr vs)]
       [len12 (+ (cadr v1) (cadr v2))]
       [i1 (index-where valves (lambda (v) (equal? (car v1) (car v))))]
       [i2 (index-where valves (lambda (v) (equal? (car v2) (car v))))]
       [V1 (list-ref valves i1)]
       [V2 (list-ref valves i2)]
       [V1_ (list-set V1 2 (map (lambda (edge) (if (equal? (car v0) (car edge)) (list (car v2) len12) edge)) (caddr V1)))]
       [V2_ (list-set V2 2 (map (lambda (edge) (if (equal? (car v0) (car edge)) (list (car v1) len12) edge)) (caddr V2)))]
       [vals1 (list-set valves i1 V1_)]
       [vals2 (list-set vals1 i2 V2_)]
       [vals3 (remove v0 vals2)])
      (simplify-valves vals3)
    )
  )
)


(define (find2 valves good-valves time-left current1 current2 visited1 visited2 open-valves open-sum released logg)
  ;(println (list (- 27 time-left) current1 current2 open-valves open-sum released))
  ;(displayln (reverse logg))
  (if (or (= 0 time-left) (= good-valves (length open-valves)))
    (begin
      ;(println (list "DONE" released (reverse logg)))
      (list (+ released (* open-sum time-left)) (reverse logg))
    )
    (let* (
           [actions1
             (if
               (equal? current1 'idle)
               '(idle)
               (let* ([valve1 (hash-ref valves current1 #f)]
                     [rate1 (car valve1)]
                     [tunnels1 (cadr valve1)])
                     (filter identity (cons
                       (if (or (= 0 rate1) (member current1 open-valves)) #f 'open)
                       (map (lambda (v) (if (member (car v) visited1) #f (car v))) tunnels1)
                       ))
                     ))]
           [actions1_ (if (empty? actions1) '(idle) actions1)]

           [actions2
             (if
               (equal? current2 'idle)
               '(idle)
               (let* ([valve2 (hash-ref valves current2 #f)]
                     [rate2 (car valve2)]
                     [tunnels2 (cadr valve2)])
                     (filter identity (cons
                       (if (or (= 0 rate2) (member current2 open-valves) (equal? current1 current2)) #f 'open)
                       (map (lambda (v) (if (member (car v) visited2) #f (car v))) tunnels2)
                       ))
                     ))]
           [actions2_ (if (empty? actions2) '(idle) actions2)]

           [actions (cartesian-product actions1_ actions2_)]

           [results (map (lambda(actions) (do-actions valves good-valves time-left current1 current2 visited1 visited2 open-valves
                                                 open-sum released logg actions)) actions)]
           )
        (argmax car results)
    )
  )
)


(define (do-actions valves good-valves time-left current1 current2 visited1 visited2 open-valves open-sum released logg actions)
  ;(println actions)
  (define a1 (car actions))
  (define a2 (cadr actions))
  (define current1_ (if (equal? a1 'open) current1 a1))
  (define current2_ (if (equal? a2 'open) current2 a2))
  (define visited1_ (if (equal? a1 'open) '() (cons a1 visited1)))
  (define visited2_ (if (equal? a2 'open) '() (cons a2 visited2)))
  (define open-valves_ (if (equal? a1 'open) (cons current1 open-valves) open-valves))
  (define open-valves__ (if (equal? a2 'open) (cons current2 open-valves_) open-valves_))
  (define open-sum_ (if (equal? a1 'open) (+ open-sum (car (hash-ref valves current1 #f))) open-sum))
  (define open-sum__ (if (equal? a2 'open) (+ open-sum_ (car (hash-ref valves current2 #f))) open-sum_))
  (define released_ (+ released open-sum))
  (define logg_ (cons (list a1 a2) logg))
  (find2 valves good-valves (- time-left 1) current1_ current2_ visited1_ visited2_ open-valves__ open-sum__ released_ logg_)
)


(define valves (map parse-valve (file->lines "16_.input")))
;(println valves)
(define good-valves (length (filter (lambda (v) (< 0 (cadr v))) valves)))
;(println good-valves)
(define valves0 (simplify-valves valves))
;(println valves0)
;(define valves0 valves)
(define valves_ (make-immutable-hash valves0))
(println valves_)

(define paths (find-paths valves_))
(println paths)

;(define res (find valves_ 30 "AA" '("AA") '() 0 0 '()))
;(displayln res)

(define res2 (find2 valves_ good-valves 26 "AA" "AA" '("AA") '("AA") '() 0 0 '()))
(displayln res2)


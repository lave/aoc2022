#lang racket

(require "common.rkt")

(define D 5)

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
    (lambda (k v) (map (lambda (p) (cons (cons k (car p)) (cdr p))) (hash->list (find-paths_ valves k 0 (hash) '()))))
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
  ;(displayln (reverse logg))
  ;(println (list (- 31 time-left) current open-valves open-sum released))
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

(define (take-safe l n)
  (if (> n (length l)) l (take l n))
)

(define (get-score valves valve dist time-left)
  (define rate (car (hash-ref valves valve)))
  (define r (* rate (- time-left dist)))
  ;(println (list valve rate dist time-left "=" r))
  r
)

(define (find2 valves paths time-left current1 current2 action1 action2 visited open-valves open-sum released logg)
  ;(displayln (reverse logg))
  ;(println (list (- 27 time-left) current1 current2 action1 action2 visited open-valves open-sum released))
  (if (or (= 0 time-left) (= (hash-count valves) (set-count open-valves)))
    (begin
      ;(println (list "DONE" released (reverse logg)))
      (list (+ released (* open-sum time-left)) (reverse logg))
    )
    (let* (
           [non-visited (filter (lambda (v) (not (set-member? visited v))) (hash-keys valves))]
           [actions1
             (if (< 0 (cdr action1))
               (list action1)
               (let* (
                     [valve1 (hash-ref valves current1 #f)]
                     [rate1 (car valve1)]
                     [tunnels1 (cadr valve1)]
                     [ac-open (if (set-member? open-valves current1) #f (cons (cons 'open 1) (* rate1 (- time-left 1))))]
                     [ac-moves (filter (lambda (v) (> time-left (cdar v)))
                                (map (lambda (v) (let ([dist (hash-ref paths (cons current1 v) #f)])
                                                   (cons (cons v dist) (get-score valves v dist time-left)))) non-visited))]
                     [ac-all (if ac-open (cons ac-open ac-moves) ac-moves)] 
                     [sorted (sort ac-all (lambda (v1 v2) (> (cdr v1) (cdr v2))))]
                     )
                     (map car (take-safe sorted D))
             ))]
           [actions1_ (if (empty? actions1) (list (cons 'idle time-left)) actions1)]

           [actions2
             (if (< 0 (cdr action2))
               (list action2)
               (let* (
                     [valve2 (hash-ref valves current2 #f)]
                     [rate2 (car valve2)]
                     [tunnels2 (cadr valve2)]
                     [ac-open (if (or (equal? current1 current2) (set-member? open-valves current2)) #f (cons (cons 'open 1) (* rate2 (- time-left 1))))]
                     [ac-moves (filter (lambda (v) (> time-left (cdar v)))
                                (map (lambda (v) (let ([dist (hash-ref paths (cons current2 v) #f)])
                                                   (cons (cons v dist) (get-score valves v dist time-left )))) non-visited))]
                     [ac-all (if ac-open (cons ac-open ac-moves) ac-moves)] 
                     [sorted (sort ac-all (lambda (v1 v2) (> (cdr v1) (cdr v2))))]
                     )
                     (map car (take-safe sorted D))
             ))]
           [actions2_ (if (empty? actions2) (list (cons 'idle time-left)) actions2)]

           [actions (cartesian-product actions1_ actions2_)]
           [actions-deduped (filter (lambda (acs) (or (equal? 'idle (caar acs)) (equal? 'open (caar acs)) (not (equal?  (car acs) (cadr acs))))) actions)]
           [actions_ (if (empty? actions-deduped) actions actions-deduped)]

           [results (map (lambda(actions2) (do-actions valves paths time-left current1 current2 visited open-valves
                                                 open-sum released logg actions2)) actions_)]
           )
        ;(if (empty? actions) (println (list "DUPS" actions1_ actions2_)) #f)
        (argmax car results)
    )
  )
)


(define (do-actions valves paths time-left current1 current2 visited open-valves open-sum released logg actions)
  ;(println actions)
  (define at1 (car actions))
  (define at2 (cadr actions))
  (define a1 (car at1))
  (define a2 (car at2))
  (define t1 (cdr at1))
  (define t2 (cdr at2))
  (define t (min t1 t2))
  (define t1_ (- t1 t))
  (define t2_ (- t2 t))
  (define at1_ (cons a1 t1_))
  (define at2_ (cons a2 t2_))
  (define current1_     (if (< 0 t1_) current1 (if (equal? a1 'open) current1 a1)))
  (define current2_     (if (< 0 t2_) current2 (if (equal? a2 'open) current2 a2)))
  (define visited_      (if (< 0 t1_) visited  (if (equal? a1 'open) visited (set-add visited a1))))
  (define visited__     (if (< 0 t2_) visited_ (if (equal? a2 'open) visited_ (set-add visited_ a2))))
  (define open-valves_  (if (< 0 t1_) open-valves  (if (equal? a1 'open) (set-add open-valves current1) open-valves)))
  (define open-valves__ (if (< 0 t2_) open-valves_ (if (equal? a2 'open) (set-add open-valves_ current2) open-valves_)))
  (define open-sum_     (if (< 0 t1_) open-sum     (if (equal? a1 'open) (+ open-sum (car (hash-ref valves current1 #f))) open-sum)))
  (define open-sum__    (if (< 0 t2_) open-sum_    (if (equal? a2 'open) (+ open-sum_ (car (hash-ref valves current2 #f))) open-sum_)))
  (define released_ (+ released (* t open-sum)))
  ;(define logg_ (cons (list at1 at2) logg))
  (define logg_ logg)
  (define time-left_ (- time-left t))
  (find2 valves paths time-left_ current1_ current2_ at1_ at2_ visited__ open-valves__ open-sum__ released_ logg_)
)


(define valves (map parse-valve (file->lines "16.input")))
;(println valves)
;(define good-valves (length (filter (lambda (v) (< 0 (cadr v))) valves)))
;(define good-valves (length valves))
;(println good-valves)
(define valves0 (simplify-valves valves))
(define open-valves (set (map car (filter (lambda (v) (= 0 (cadr v))) valves0))))
;(println valves0)
;(define valves0 valves)
(define valves_ (make-immutable-hash valves0))
(println valves_)
(println (hash-count valves_))

(define paths (find-paths valves_))
(println paths)

;(define res (find valves_ 30 "AA" '("AA") '() 0 0 '()))
;(displayln res)

(define res2 (find2 valves_ paths 26 "AA" "AA" (cons 'init 0) (cons 'init 0) (set "AA") open-valves 0 0 '()))
(displayln res2)


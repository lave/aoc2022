#lang racket

(require racket/hash)

(define (parse-dot s)
  (map string->number (string-split s ","))
)

(define (count-open-sizes dset p)
  (define ps (neighboors p))
  (define is-open (map (lambda (p_) (set-member? dset p_)) ps))
  (define count (length (filter not is-open)))
  count
)

(define (neighboors p)
  (define-values (x y z) (apply values p))
  (define ps (list (list (- x 1) y z) (list (+ x 1) y z) (list x (- y 1) z) (list x (+ y 1) z) (list x y (- z 1)) (list x y (+ z 1))))
  ps
)

(define (find-inside-ps dset bounds p visited)
  ;(println (list p visited))
  (define-values (x y z) (apply values p))
  (define-values (xmin xmax ymin ymax zmin zmax) (apply values bounds))
  (define res
  (if (or (not visited) (< x xmin) (> x xmax) (< y ymin) (> y ymax) (< z zmin) (> z zmax))
    #f
    (if (or (set-member? dset p) (set-member? visited p))
      visited
      (let* ([visited_ (set-add visited p)]
            [ps (neighboors p)])
        (foldl (lambda (p_ visited__)
          (if visited__
            (find-inside-ps dset bounds p_ visited__)
            #f
          )
        ) visited_ ps)
      )
    )
  ))
  ;(println (list "RES " res))
  res
)

(define (is-inside dset bounds p inside)
  (define cached (hash-ref inside p 'none))
  (define res
    (if (equal? cached 'none)
      (let ([ps (find-inside-ps dset bounds p (set))])
        (if (not ps)
          (list #f (hash-set inside p #f))
          (list #t (hash-union inside (make-hash (set-map ps (lambda (e) (cons e #t))))))
        )
      )
      (list cached inside)
    )
  )
  ;(println (list "DONE" res))
  res
)

(define (count-open-sizes-outside dset bounds p state)
  ;(println (list p state))
  (define ps (neighboors p))
  (define result (foldl (lambda (p_ state_)
    (define-values (n inside) (apply values state_))
    (if (set-member? dset p_)
      (list n inside)
      (let ([res (is-inside dset bounds p_ inside)])
        (list (+ n (if (car res) 0 1)) (cadr res))
      )
    ))
    state
    ps))
  result
)

(define dropplet (map parse-dot (file->lines "18.input")))
;(println dropplet)

(define xmin (apply min (map car dropplet)))
(define xmax (apply max (map car dropplet)))
(define ymin (apply min (map cadr dropplet)))
(define ymax (apply max (map cadr dropplet)))
(define zmin (apply min (map caddr dropplet)))
(define zmax (apply max (map caddr dropplet)))
(define bounds (list xmin xmax ymin ymax zmin zmax))
(println bounds)

(define dset (list->set dropplet))
(define open-sides (map (lambda (p) (count-open-sizes dset p)) dropplet))
;(define res1 (apply + open-sides))
;(displayln res1)

(define res2 (foldl (lambda (p inside) (count-open-sizes-outside dset bounds p inside)) (list 0 (hash)) dropplet))
(displayln (car res2))

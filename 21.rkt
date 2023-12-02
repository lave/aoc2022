#lang racket

(require "common.rkt")

(define (parse-monkey s)
  (define ts (string-split s ": "))
  (define name (car ts))
  (define expr (cadr ts))
  (define num (string->number expr))
  (if num
    (cons name num)
    (cons name (parse-expr expr))
  )
)

(define (parse-expr s)
  (define-values (l op r) (apply values (string-split s)))
  (define op_ (case op [("+") '+] [("-") '-] [("*") '*] [("/") '/]))
  (list op_ l r)
)

(define (op_ op)
  (define res (case op ['+ +] ['- -] ['* *] ['/ /]))
  ;(println (list op res))
  res
)

(define (evaluate m)
  (define nums (make-hash (filter (lambda (p) (number? (cdr p))) (hash->list m))))
  ;(println nums)
  (define res (foldl (lambda (p state)
                       (if (number? (cdr p))
    state
    (let* ([op (op_ (car (cdr p)))]
           [l (cadr (cdr p))]
           [r (caddr (cdr p))]
           [l_ (hash-ref nums l #f)]
           [r_ (hash-ref nums r #f)])
      (if (and l_ r_)
        (begin
          (hash-set! (car state) (car p) (op l_ r_))
          (list (car state) #t)
        )
        state
      )
    )
  ))
  (list m #f)
  (hash->list m)))

  (if (cadr res)
    (evaluate (car res))
    (car res)
  )
)

(define (find-path m humn cur path)
  ;(println (list cur path (hash-ref m cur)))
  (if (equal? "humn" cur)
    (reverse path)
    (let ([mnk (hash-ref m cur)])
      (if (number? mnk)
        #f
        (let* ([op (car mnk)]
               [l (cadr mnk)]
              [r (caddr mnk)]
              [l_ (find-path m humn l (cons (list cur op 'l r) path))]
              [r_ (find-path m humn r (cons (list cur op 'r l) path))])
          (if l_ l_ r_)
        )
      )
    )
  )
)

(define (find-val m path v)
  ;(println (list (car path) v))
  (if (empty? path)
    v
    (let* ([cur (car path)]
           [other (cadddr cur)]
           [op (cadr cur)]
           [pos (caddr cur)]
           [v1 (hash-ref m other)]
           [v_ (case op
                 ['+ (- v v1)]
                 ['- (if (equal? 'l pos) (+ v v1) (- v1 v))]
                 ['* (/ v v1)]
                 ['/ (if (equal? 'l pos) (* v v1) (/ v1 v))]
                )
               ])
      (begin
        ;(println (list other v1))
        (find-val m (cdr path) v_)
      )
    )
  )
)



(define monkeys (map parse-monkey (file->lines "21.input")))
(define m (make-hash monkeys))
;(println m)

(define m_ (evaluate m))
(println m_)
(define res1 (hash-ref m_ "root"))
(println res1)

(define m0 (make-hash monkeys))
(define path (find-path m0 "humn" "root" '()))
(println (length path))

(define p0 (car path))
(define v0 (hash-ref m_ (cadddr p0))) 
(define res2 (find-val m_ (cdr path) v0))
(println res2)

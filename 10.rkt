#lang racket

(require "common.rkt")

(define (parse-cmd line)
  (define tokens (string-split line))
  (define cmd (car tokens))
  (case cmd
    [("noop") (list 'noop)]
    [("addx") (list 'addx (string->number (cadr tokens)))]
  )
)

(define (get-sprite reg n)
  (define x (modulo (- n 1) 40))
  (if (< (abs (- x reg)) 2) #\# #\.)
)

(define (op-cycles op)
  (if (equal? op 'noop) 1 2)
)

(define (simulate cmds n)
  (simulate_ (list (op-cycles (car cmds)) 1 '()) cmds n)
)

(define (simulate_ state cmds n)
  (define-values (cycles-to-cmd-end reg reg-history) (apply values state))
  (if (or (= 0 n) (empty? cmds))
    (reverse reg-history)
    (let* ([cmd (car cmds)]
           [op (car cmd)])
      (if (= cycles-to-cmd-end 0)
        (simulate_
          (list
            (op-cycles op)
            (if (equal? op 'noop) reg (+ reg (cadr cmd)))
            reg-history)
          (cdr cmds)
          n)
        (simulate_
          (list
            (- cycles-to-cmd-end 1)
            reg
            (cons reg reg-history))
          cmds
          (- n 1))
      )
    )
  )
)


(define cmds (map parse-cmd (file->lines "10.input")))
;(println cmds)

(define reg-vals (simulate cmds 240))
;(println reg-vals)

(define cycles (range 1 (+ 1 (length reg-vals))))
(define signal-strengths (map (lambda (reg n) (* reg n)) reg-vals cycles))
(define selected (map (lambda (i) (list-ref signal-strengths (- i 1))) '(20 60 100 140 180 220)))
(displayln (apply + selected))

(define screen (map get-sprite reg-vals cycles))
(define lines (map list->string (split-by screen 40)))
(for-each displayln lines)

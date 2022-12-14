#lang racket

(require "common.rkt")

(define (parse-monkey lines)
  (define items (map string->number (string-split (cadr (string-split (list-ref lines 1) ": ")) ", ")))
  (define operation (parse-operation (cadr (string-split (list-ref lines 2) "new = "))))
  (define divider (string->number (last (string-split (list-ref lines 3)))))
  (define monkey-yes (string->number (last (string-split (list-ref lines 4)))))
  (define monkey-no (string->number (last (string-split (list-ref lines 5)))))
  (list 0 items operation divider monkey-yes monkey-no)
)

(define (parse-operation s)
  (define-values (arg1 op arg2) (apply values (string-split s)))
  (lambda (x) (
    (case op [("+") +] [("*") *])
    (if (equal? arg1 "old") x (string->number arg1))
    (if (equal? arg2 "old") x (string->number arg2))
  ))
)

(define (simulate has-worry-cooldown? monkeys n)
  (define monkeys_ (foldl (lambda (i ms) (simulate-round has-worry-cooldown? ms)) monkeys (range n)))
  ;(println monkeys_)
  (define business (map (lambda (l) (list-ref l 0)) monkeys_))
  (apply * (take (sort business >) 2))
)

(define (simulate-round has-worry-cooldown? monkeys)
  (foldl (lambda (i ms) (simulate-monkey has-worry-cooldown? i ms)) monkeys (range (length monkeys)))
)

(define (simulate-monkey has-worry-cooldown? i monkeys)
  (define monkey (list-ref monkeys i))
  (define activity (car monkey))
  (define items (cadr monkey))
  (define monkeys_ (foldl (lambda (i ms) (move-item has-worry-cooldown? ms monkey i)) monkeys items))
  (list-set monkeys_ i (append (list (+ activity (length items)) '()) (cddr monkey)))
)

(define (move-item has-worry-cooldown? monkeys monkey item)
  (define-values (operation divider monkey-yes monkey-no) (apply values (cddr monkey)))
  (define worry-level (if has-worry-cooldown?
    (quotient (operation item) 3)
    (modulo (operation item) 9699690)))
  (define monkey-index (if (= 0 (modulo worry-level divider)) monkey-yes monkey-no))
  (define target-monkey (list-ref monkeys monkey-index))
  (define new-items (append (cadr target-monkey) (list worry-level)))
  (define new-target-monkey (list-set target-monkey 1 new-items))
  (list-set monkeys monkey-index new-target-monkey)
)


(define monkeys (map parse-monkey (split-by-separator (file->lines "11.input") empty-string?)))
;(println monkeys)

(define monkey-business-1 (simulate #t monkeys 20))
(displayln monkey-business-1)

(define monkey-business-2 (simulate #f monkeys 10000))
(displayln monkey-business-2)

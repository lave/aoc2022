#lang racket

(provide create-matrix lists->matrix matrix->lists matrix->list matrix-dims matrix-map matrix-ref matrix-ref2 matrix-set
         matrix-set2)

(define (create-matrix w h v)
  (build-vector h (lambda (y) (build-vector w (const v))))
)

(define (lists->matrix l)
  (list->vector (map list->vector l))
)

(define (matrix->lists m)
  (map vector->list (vector->list m))
)

(define (matrix->list m)
  (apply append (matrix->lists m))
)

(define (matrix-dims m)
  (values (vector-length (vector-ref m 0)) (vector-length m))
)

(define (matrix-map f m)
  (define-values (w h) (matrix-dims m))
  (vector-map
    (lambda (y v) (vector-map
      (lambda (x e) (f x y e))
      (list->vector (range w))
      v
    ))
    (list->vector (range h))
    m
  )
)

(define (matrix-ref m x y)
  (vector-ref (vector-ref m y) x)
)

(define (matrix-ref2 m coord)
  (matrix-ref m (car coord) (cadr coord))
)

(define (matrix-set m x y v)
  (vector-set! (vector-ref m y) x v)
  m
)

(define (matrix-set2 m coord v)
  (vector-set! (vector-ref m (cadr coord)) (car coord) v)
  m
)


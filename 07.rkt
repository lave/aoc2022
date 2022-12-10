#lang racket

(define (parse-fs lines path cur-dir)
  (if (empty? lines)
    (list cur-dir lines)
    (let ([lines_ (cdr lines)]
          [ts (string-split (car lines))])
      (case (car ts)
        [("$")
          (case (cadr ts)
            [("ls")
              ; after ls command we create new hash to store directory content
              (parse-fs lines_ path (hash))]
            [("cd")
             (let ([dir (caddr ts)])
               (if (equal? dir "..")
                 ; cd .. - just return current directory content, it has all the files and directories listed by
                 ; previous ls
                 (list cur-dir lines_)
                 ; cd <dir> - recursively parse child directory, and replace empty hash in the current directory's
                 ; content with hash with actial content of the child's directory
                 (match (parse-fs lines_ (cons (caddr ts) path) (hash))
                   [(list sub-dir lines_) (parse-fs lines_ path (hash-set cur-dir dir sub-dir))])
                 ))
            ]
          )]
        [("dir")
          ; ls output - if it's a directory, add new empty hash for it to the current directory's content
          (parse-fs lines_ path (hash-set cur-dir (cadr ts) (hash)))]
        [else
          ; ls output - if it's a directory, add it (with size) to the current directory's content
          (parse-fs lines_ path (hash-set cur-dir (cadr ts) (string->number (car ts))))]
      )
    )
  )
)

; calculate size of all directories in the tree and cache it in 'size property
(define (calc-dir-sizes cur-dir)
  ; create new copy if directory (represented as hash) and apply this funcion reqursively to all sub-directories
  (define new-dir (hash-map/copy cur-dir
    (lambda (name obj) (values name (if (number? obj) obj (calc-dir-sizes obj))))
  ))
  ; get sizes of all files and sub-directories (they already have 'size property defined)
  (define sizes (hash-map new-dir (lambda (name obj) (if (number? obj) obj (hash-ref obj 'size)))))
  ; set 'size property for current directory
  (hash-set new-dir 'size (apply + sizes))
)

; get sized of all directories in the tree as list
(define (get-dir-sizes cur-dir)
  (define sub-dirs-sizes (hash-map cur-dir (lambda (name obj) (if (number? obj) '() (get-dir-sizes obj)))))
  (cons (hash-ref cur-dir 'size) (apply append sub-dirs-sizes))
)

(define lines (file->lines "07.input"))
(define fs (car (parse-fs lines '() (hash))))
;(println fs)
(define fs-with-sizes (calc-dir-sizes fs))
;(println fs-with-sizes)
(define dir-sizes (get-dir-sizes fs-with-sizes))
;(println dir-sizes)

(define sum (apply + (filter (lambda (s) (<= s 100000)) dir-sizes)))
(displayln sum)

(define total-size (hash-ref fs-with-sizes 'size))
(define need-to-free (- 30000000 (- 70000000 total-size)))
(define candidates-to-delete (sort (filter (lambda (s) (>= s need-to-free)) dir-sizes) <))
(displayln (car candidates-to-delete))

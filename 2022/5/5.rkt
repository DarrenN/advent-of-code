#lang racket/base

(require racket/list
         racket/match)

;; Store crates in silos (lists) in a vector (n-index)
(define *crates* (make-parameter (make-vector 9 '())))

(define (crate? s)
  (regexp-match #rx"\\[" s))

(define (move? s)
  (regexp-match #rx"move" s))

;; Slot crates into the correct silos, line by line
;; Pattern match to figure out if this is a crate or "empty space"
(define (parse-crate s)
  (define (m x)
    (match x
      [(list #\[ n #\]) n]
      [(list #\space #\space #\space) 'empty]))
  (define (loop l pos)
    (cond [(null? l) 'null]
          [(eq? 'empty (m (take l 3)))
           (loop (drop l (if (> (length l) 3) 4 3)) (add1 pos))]
          [else
           (vector-set! (*crates*) pos
                        (append (vector-ref (*crates*) pos) `(,(cadr l))))
           (loop (drop l (if (> (length l) 3) 4 3)) (add1 pos))]))
  (loop (string->list s) 0))

(define (move-crate s)
  (let* ([rx (regexp-match #px"move ([\\d]+) from ([\\d]+) to ([\\d]+)" s)]
         [ns (map string->number (cdr rx))]
         [q (car ns)] ; how many crates to move
         [s (sub1 (cadr ns))] ; start crate
         [e (sub1 (caddr ns))] ; end crate
         [cs (take (vector-ref (*crates*) s) q)]) ; crates to move
    ;; remove cs from the start silo
    (vector-set! (*crates*) s
                 (drop (vector-ref (*crates*) s) q))

    ;; move crates from cs one at a time to e
    (for ([c (in-list cs)])
      (vector-set! (*crates*) e
                   (append `(,c) (vector-ref (*crates*) e))))))

(define (get-tops)
  (list->string
   (for/fold ([tops '()])
             ([cs (in-vector (*crates*))])
     (if (not (null? cs))
         (append tops `(,(car cs)))
         tops))))

(define (parse in)
  (define line (read-line in))
  (cond [(eof-object? line) (get-tops)] ; print tops of each silo
        [(crate? line) (parse-crate line) (parse in)] ; parse crate into silos
        [(move? line) (move-crate line) (parse in)] ; move crates!
        [else (parse in)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require (only-in racket/file file->lines))

  (define in (open-input-string "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"))

  ;(parse in)
  (call-with-input-file "./input.txt" parse)

  )

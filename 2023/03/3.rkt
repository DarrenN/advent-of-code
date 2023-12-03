#lang racket/base

(require racket/hash
         racket/list
         racket/string)

#|

Notes:

Could re-do 3b to create the lookup table of numbers AND also capture the
(x y) location of all gears. This should shorten the lookup time on finding
neighbors as we already know where the gears are.

gears: `((x y) (x y) ...)
lookup: #hash((x y) . 123)

(for ([gear (in-list gears)])
  (find-neighbors t gear)
  ...)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 3a

#|

Scan row by char
if pos is numeric? add to buf
if part is #t don't scan neighbors (we already hit a symbol)
if part is #f scan neighbors, if symbol found set part to #t
if pos is not numeric?
if buf and part is #t then convert buff to number and add to sum
set part to #f
set buf to empty
add1 to x
if pos == last char (vector length)

Neighbors:

N  [y-1][x]
NE [y-1][x+1]
E  [y][x+1]
SE [y+1][x+1]
S  [y+1][x]
SW [y+1][x-1]
W  [y][x-1]
NW [y-1][x-1]

Track:

sum  - sum of part numbers
buf  - number buffer
x    - col pos
y    - row pos
part - bool? is part or not

|#

;; Build a 2d vector grid of char?
(define (parse-input ls)
  (for/vector ([l (in-list ls)])
    (list->vector (string->list l))))

;; Scan the rows looking for numbers and neighbors.
(define (find-parts t)
  (define neighbor? (find-neighbor t))
  (define (has-neighbor? x y)
    (or
     (neighbor? x (sub1 y) ) ;; N
     (neighbor? (add1 x) (sub1 y)) ;; NE
     (neighbor? (add1 x) y) ;; E
     (neighbor? (add1 x) (add1 y)) ;; SE
     (neighbor? x (add1 y)) ;; S
     (neighbor? (sub1 x) (add1 y)) ;; SW
     (neighbor? (sub1 x) y) ;; W
     (neighbor? (sub1 x) (sub1 y))))

  (for/fold ([s 0])
            ([row (in-vector t)]
             [y (in-range (vector-length t))])
    (define-values (buf part? sum)
      (for/fold ([buf '()]
                 [part? #f]
                 [sum 0])
                ([pos (in-vector row)]
                 [x (in-range (vector-length row))])
        (cond
          [(char-numeric? pos)
           (cond
             ;; we're a part and we're at the end of the line
             [(and part? (equal? x (sub1 (vector-length row))))
              (values '() #f
                      (+ sum
                         (string->number
                          (apply string (append buf `(,pos))))))]
             ;; if part? we don't need to scan neighbors
             [part? (values (append buf `(,pos)) part? sum)]
             [else
              ;; if we have a symbol neighbor then we're a part?
              (if (has-neighbor? x y)
                  (values (append buf `(,pos)) #t sum)
                  (values (append buf `(,pos)) #f sum))])]
          [(and (not (null? buf)) part?)
           (values '() #f
                   (+ sum (string->number (apply string buf))))]
          [else ;; period or symbol, if we have a buffer & part then sum
           (values '() #f sum)])))
    (+ s sum)))

(define (extract-value bs)
  (for/fold ([v '()]
             #:result (string->number (apply string v)))
            ([b (in-list bs)])
    (append v `(,(third b)))))

;; Part 3b uses a two-pass solution. First we scan the table looking for numbers
;; which we add to a look-up table (hash) keyed on (x y) coordinates. In the
;; second pass we look for neighbors of * and pull the values from this lookup
;; table. This has the downside of eating a lot of space for the lookup, as its
;; full of dupes.
(define (find-numbers t)
  (define h
    (for/fold ([lookup (hash)])
              ([row (in-vector t)]
               [y (in-range (vector-length t))])
      (define-values (buf look)
        (for/fold ([buf '()]
                   [look lookup])
                  ([pos (in-vector row)]
                   [x (in-range (vector-length row))])
          (cond
            [(and (char-numeric? pos) (equal? x (sub1 (vector-length row))))
             (let* ([bs (append buf `((,x ,y ,pos)))]
                    [v (extract-value bs)]
                    [h (for/hash ([b bs])
                         (values `(,(car b) ,(cadr b)) v))])
               (values '() (hash-union look h)))]
            [(char-numeric? pos)
             (values (append buf `((,x ,y ,pos))) look)]
            [(and (not (null? buf)) (not (equal? pos #\*)))
             (let* ([bs buf]
                    [v (extract-value bs)]
                    [h (for/hash ([b bs])
                         (values `(,(car b) ,(cadr b)) v))])
               (values '() (hash-union look h)))]
            ;; handle gear marker
            [(and (not (null? buf)))
             (let* ([bs buf]
                    [v (extract-value bs)]
                    [h (for/hash ([b bs])
                         (values `(,(car b) ,(cadr b)) v))])
               (values '()
                       (hash-union look h)))]
            [else (values '() look)])))
      look))
  (values t h))

;; Second pass for 3b: looks for gear markers and then pulls any neighbors from
;; the lookup table by their x,y coordinates. If there are exactly 2
;; neighbors and they're diff values, multiple them
(define (find-gears t look)
  (define neighbor? (find-gear-neighbor t))
  (define (check-neighbors x y)
    (filter
     (λ (x) x)
     `(,(neighbor? x (sub1 y) ) ;; N
       ,(neighbor? (add1 x) (sub1 y)) ;; NE
       ,(neighbor? (add1 x) y) ;; E
       ,(neighbor? (add1 x) (add1 y)) ;; SE
       ,(neighbor? x (add1 y)) ;; S
       ,(neighbor? (sub1 x) (add1 y)) ;; SW
       ,(neighbor? (sub1 x) y) ;; W
       ,(neighbor? (sub1 x) (sub1 y)))))

  (for/fold ([gs '()])
            ([row (in-vector t)]
             [y (in-range (vector-length t))])
    (define g
      (for/fold ([gears '()])
                ([pos (in-vector row)]
                 [x (in-range (vector-length row))])
        (cond
          [(equal? pos #\*)
           (let* ([ns (check-neighbors x y)]
                  [vs (for/hash ([n (in-list ns)])
                        (values (hash-ref look `(,(car n) ,(cadr n))) #t))]
                  [ks (hash-keys vs)])
             (if (and (> (length ks) 1)
                      (< (length ks) 3)
                      (not (eq? (car ks) (cadr ks))))
                 (append gears `(,(* (car ks) (cadr ks))))
                 gears))]
          [else gears])))
    (append gs g)))

;; Predicate for not numeric and not a .
(define (part-symbol? c)
  (not (or (char-numeric? c) (eqv? c #\.))))

;; Do some bounds checking before accessing vector elements,
;; Use this to generate functions bound to the 2d matrix.
(define (find-neighbor t)
  (define max-row (vector-length t))
  (define max-col (vector-length (vector-ref t 0)))
  (λ (x y)
    (cond
      [(or (< x 0) (< y 0) (> x (sub1 max-col)) (> y (sub1 max-row))) #f]
      [else (part-symbol? (vector-ref (vector-ref t y) x))])))

(define (find-gear-neighbor t)
  (define max-row (vector-length t))
  (define max-col (vector-length (vector-ref t 0)))
  (λ (x y)
    (cond
      [(or (< x 0) (< y 0) (> x (sub1 max-col)) (> y (sub1 max-row))) #f]
      [else
       (if (char-numeric? (vector-ref (vector-ref t y) x))
           `(,x ,y)
           #f)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test

  (require rackunit
           racket/string
           racket/list
           (only-in racket/file file->lines))

  (define (prep-test-input s)
    (string-split s #px"\n"))

  (define test-input "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

  ;; 3b
  (define-values (t look)
    (find-numbers (parse-input (file->lines "./input.txt"))))
  (apply + (find-gears t look))

  ;; 3a
  ;;(find-parts (parse-input (file->lines "./input.txt")))

  )

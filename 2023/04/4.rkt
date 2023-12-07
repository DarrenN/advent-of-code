#lang racket/base

(require racket/hash
         racket/list
         racket/string)

#|

Advent of Code Day 4 https://adventofcode.com/2023/day/4

Notes:

Store numbers in a hasheq for O(1) lookup, the winning numbers
can stay in a list as you're going to iterate over them anyway.

Or, use hash-intersect to combine two hashes:
(hash-intersect ha hb #:combine (λ (v1 v2) #t))

Creating hasheq (apply hasheq '(1 a 2 b 3 c))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 4a

(define (->strnum l)
  (string->number (apply string l)))

;; Make sure we got the last number in the card
(define (store-value buf wins nums divider)
  (if (not (null? buf))
      `(,wins ,(hash-set nums (->strnum buf) #t))
      `(,wins ,nums)))

;; Find the geometric progression of winning numbers in a card
(define (calc-winners vs)
  (define ws (car vs))
  (define ns (cadr vs))
  (define a 1)
  (define r 2)
  (define ks (hash-keys (hash-intersect ws ns #:combine (λ (v1 v2) #t))))
  (if (zero? (length ks))
      0
      (last
       (for/list ([i (in-range a (add1 (length ks)))])
         (* a (expt r (- i 1)))))))

;; Scan the card line by char and store wins & nums in hashes which we can
;; intersect to find the winners.
(define (scan-input l)
  (for/fold ([buf '()]
             [wins (hasheq)]
             [nums (hasheq)]
             [divider #f]
             #:result (calc-winners (store-value buf wins nums divider)))
            ([c (in-list (string->list l))])
    (cond
      ;; Treat the : as a signal to start capturing numbers, ditch all preceding
      [(equal? c #\:) (values '() wins nums #\:)]
      ;; Divider between wins & nums, switch on the divider sigil
      [(equal? c #\|) (values buf wins nums #\|)]
      ;; Treat spaces as dividers between numbers signalling a capture
      ;; Capture numbers into either wins or nums depending on divider
      [(equal? c #\space)
       (cond
         [(and (equal? divider #\:) (not (null? buf)))
          (values '() (hash-set wins (->strnum buf) #t) nums divider)]
         [(and (equal? divider #\|) (not (null? buf)))
          (values '() wins (hash-set nums (->strnum buf) #t) divider)]
         [else
          (values '() wins nums divider)])]
      ;; Capture numbers into the buffer
      [(char-numeric? c)
       (values (append buf `(,c)) wins nums divider)]
      ;; Ignore everything else
      [else (values (append buf `(,c)) wins nums divider)])))

(define (find-winners ls)
  (for/fold ([ws '()]
             #:result (apply + ws))
            ([l (in-list ls)])
    (append ws `(,(scan-input l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 4b

(define (find-winning-cards ls)
  (define len (length ls)) ;; this is our stopping point
  (define cards
    (for/fold ([cs (hasheq)])
              ([l (in-list ls)])
      (define-values (id children) (scan-input-b l len))
      (hash-set cs id children)))
  cards)

(define (calc-children vs len)
  (define id (car vs))
  (define ws (cadr vs))
  (define ns (caddr vs))
  (define ks (hash-keys (hash-intersect ws ns #:combine (λ (v1 v2) #t))))
  (cond
    ;;[(eq? id len) ;; never run past the end of the table
    ;; (values id '())]
    [(zero? (length ks))
     (values id '())]
    [else
     (values id (for/list ([i (in-range (length ks))]) (+ (add1 id) i)))]))

;; Make sure we got the last number in the card
(define (store-values buf id wins nums divider)
  (if (not (null? buf))
      `(,id ,wins ,(hash-set nums (->strnum buf) #t))
      `(,id ,wins ,nums)))

(define (scan-input-b l len)
  (for/fold ([buf '()]
             [id 0]
             [wins (hasheq)]
             [nums (hasheq)]
             [divider #f]
             #:result (calc-children
                       (store-values buf id wins nums divider) len))
            ([c (in-list (string->list l))])
    (cond
      ;; Treat the : as a signal to start capturing numbers, ditch all preceding
      [(equal? c #\:) (values '() (->strnum buf) wins nums #\:)]
      ;; Divider between wins & nums, switch on the divider sigil
      [(equal? c #\|) (values buf id wins nums #\|)]
      ;; Treat spaces as dividers between numbers signalling a capture
      ;; Capture numbers into either wins or nums depending on divider
      [(equal? c #\space)
       (cond
         [(and (equal? divider #\:) (not (null? buf)))
          (values '() id (hash-set wins (->strnum buf) #t) nums divider)]
         [(and (equal? divider #\|) (not (null? buf)))
          (values '() id wins (hash-set nums (->strnum buf) #t) divider)]
         [else
          (values '() id wins nums divider)])]
      ;; Capture numbers into the buffer
      [(char-numeric? c)
       (values (append buf `(,c)) id wins nums divider)]
      ;; Ignore everything else
      [else (values (append buf `(,c)) id wins nums divider)])))


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
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

  ;; 4a
  ;;(find-winners (prep-test-input test-input))
  ;;(find-winners (file->lines "./input.txt"))

  ;; 4b
  ;(define cards (find-winning-cards (prep-test-input test-input)))
  (define cards (find-winning-cards (file->lines "./input.txt")))

  
  (define ct 0)

  (define (count-cards ids)
    (for ([i ids])
      (define cs (hash-ref cards i))
      (cond
        [(not (null? cs))
         (set! ct (+ ct 1))
         (count-cards cs)]
        [else (set! ct (+ ct 1))])))

  (count-cards (in-inclusive-range 1 (length (hash-keys cards))))

  ct
  ; 
  )

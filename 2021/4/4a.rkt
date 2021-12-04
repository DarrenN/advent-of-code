#lang racket/base

#|
Bingo

data structures:

(define point (struct (x y val [called? #f]) #:transparent))

(define board
(hash 'seen '(.. list of numbers? called)
    22 (point 0 0 22 #f)
    12 (point 0 1 12 #f))

(define boards (vector ...board))

when a number is "called":

vector-map over each board in boards
if board has key (called number)
  update point for key with called? #t and add to 'seen list
  if (length seen) >= 5 then check for sequences that "win"

check for wins:


|#

(require racket/function)

(struct point (x y v [called? #:auto #:mutable])
  #:auto-value #f
  #:transparent)

(struct board ([seen #:auto #:mutable] [points #:auto #:mutable])
  #:auto-value (hash)
  #:transparent)

(define (make-board)
  (board))

;; hash keyed on point-y
(define (check-across ps)
  (define sorted
    (for/fold ([h (hash)])
              ([p (in-list ps)])
      (hash-update h (point-y p) (curry append (list p)) '())))
  (for/or ([(_ v) (in-hash sorted)])
    (>= (length v) 5)))

(define (check-down ps)
  (define sorted
    (for/fold ([h (hash)])
              ([p (in-list ps)])
      (hash-update h (point-x p) (curry append (list p)) '())))
  (for/or ([(_ v) (in-hash sorted)])
    (>= (length v) 5)))

(define (check-down-or-across ps)
  (or (check-down ps) (check-across ps)))

(define (check-board b)
  (define points (board-points b))
  (define ps (for/list ([k (in-list (hash-keys (board-seen b)))])
               (hash-ref points k)))
  (check-down-or-across ps))

(define (add-point b p)
  (set-board-points! b (hash-set (board-points b) (point-v p) p)))

(define (call-number b n)
  (when (hash-has-key? (board-points b) n)
    (set-point-called?! (hash-ref (board-points b) n) #t)
    (set-board-seen! b (hash-set (board-seen b) n #t))
    (when (>= (length (hash-keys (board-seen b))) 5)
      (printf "Winning board? ~a~n" (check-board b)))
    ))

(module+ main)

(module+ test
  (require racket/string)

  (define across (for/list ([i (in-range 5)])
                   (point i 2 (* 2 i))))

  (define down (for/list ([i (in-range 5)])
                 (point 3 i (* 3 i))))

  (define (rand)
    (for/list ([i (in-range 5)])
      (point (random 5) (random 5) (* 4 i))))

  (define c (append (rand) across))
  (check-down-or-across c)

  (define p (point (random 5) (random 5) (random 50)))
  (define b (make-board))
  (add-point b p)

  (define calls
    (list (random 50) 11 4 (random 50) (random 50 )16 18 (random 50) 15))
  
  (define input #<<BRD
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19
BRD
)

  (define board-inputs (map string-split (string-split input "\n")))
  (define brd (make-board))
  (for ([row (in-list board-inputs)]
        [y (in-range (length board-inputs))])
    (for ([v (in-list row)]
          [x (in-range (length row))])
      (void
       (add-point brd (point x y (string->number v))))))
  
  (map (curry call-number brd) calls)
  
  )

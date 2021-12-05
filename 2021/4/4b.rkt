#lang racket/base

(require (only-in racket/file
                  file->lines)
         (only-in racket/list
                  last)
         (only-in racket/function
                  curry
                  identity)
         (only-in racket/string
                  string-split)
         (only-in "../lib/utils.rkt"
                  group)
         (only-in "../lib/timed.rkt"
                  print-timed))

;; Advent of Code 2021 Day 4 Part 1

;; A "number" on a board. Stores its x / y position on the grid
;; v is its value
;; called? has it been called on this board or not?
(struct point (x y v [called? #:auto #:mutable])
  #:auto-value #f
  #:transparent)

;; Boards are stores as two hashes (maps)
;; bingo? = stop working on this board if has key 'won
;; seen = hash<value><bool> is a collection of calls that were on this board
;; points = hash<value><point> is a collection of all the points on a board and
;; their status
(struct board ([bingo? #:auto #:mutable]
               [seen #:auto #:mutable]
               [points #:auto #:mutable])
  #:auto-value (hash)
  #:transparent)

;; Check for 5 points across the grid
;; hash keyed on point-y
(define (check-across ps)
  (define sorted
    (for/fold ([h (hash)])
              ([p (in-list ps)])
      (hash-update h (point-y p) (curry append (list p)) '())))
  (for/or ([(_ v) (in-hash sorted)])
    (>= (length v) 5)))

;; Check for 5 points down the grid
;; hash keyed on point-x
(define (check-down ps)
  (define sorted
    (for/fold ([h (hash)])
              ([p (in-list ps)])
      (hash-update h (point-x p) (curry append (list p)) '())))
  (for/or ([(_ v) (in-hash sorted)])
    (>= (length v) 5)))

(define (check-down-or-across ps)
  (or (check-down ps) (check-across ps)))

;; Is the board in a bingo? state
;; Ignore boards that are already won
(define (check-board b)
  (if (hash-has-key? (board-bingo? b) 'won)
      #f
      (let* ([points (board-points b)]
             [ps (for/list ([k (in-list (hash-keys (board-seen b)))])
                   (hash-ref points k))])
        (check-down-or-across ps))))

(define (add-point b p)
  (set-board-points! b (hash-set (board-points b) (point-v p) p)))

;; Sum all uncalled points on a board and multiple by the last called number
(define (sum-board b n)
  (define unmarked
    (for/sum ([p (in-list (hash-values (board-points b)))])
      (if (not (point-called? p))
          (point-v p)
          0)))
  ;; Return sum of unmarked x last called number
  (* unmarked n))

;; If the called numbers exists in the board add it to seen
;; If seen is 5 or greater in length, check board for bingo!
(define (call-number b n)
  (if (hash-has-key? (board-points b) n)
      (begin
        (set-point-called?! (hash-ref (board-points b) n) #t)
        (set-board-seen! b (hash-set (board-seen b) n #t))
        (if (>= (length (hash-keys (board-seen b))) 5)
            (if (check-board b)
                (begin
                  ; Mark won boards so we can ignore on more runs
                  (set-board-bingo?! b (hash-set (board-bingo? b) 'won #t))
                  (sum-board b n))
                #f)
            #f))
      #f))

;; Convert input into calls and rows
(define (parse-input input-list)
  (define calls (car input-list))
  (define boards
    (for/fold ([bs '()])
              ([b (in-list (group (cdr input-list) '() 6))])
      (cons (map string-split (cdr b)) bs)))
  (values (map string->number (string-split calls ",")) boards))

;; Load a board with points
(define (create-board rows)
  (define b (board))
  (for ([row (in-list rows)]
        [y (in-range (length rows))])
    (for ([v (in-list row)]
          [x (in-range (length row))])
      (void
       (add-point b (point x y (string->number v))))))
  b)

(define (create-boards board-list)
  (for/list ([board (in-list board-list)])
    (create-board board)))

(define (play-bingo rows calls)
  (define boards (create-boards rows))
  (for/or ([call (in-list calls)])
    (for/or ([board (in-list boards)])
      (call-number board call))))

(define (play-bingo-last rows calls)
  (define boards (create-boards rows))
  (define winners
    (for/fold ([winners '()])
              ([call (in-list calls)])
      (let ([bingo? (for/list ([board (in-list boards)])
                      (call-number board call))])
        (if (not (null? bingo?))
            (append winners bingo?)
            winners))))
  (last (filter identity winners)))


(module+ main
  (define-values (calls rows)
    (parse-input (file->lines "./input.txt")))
  (print-timed play-bingo-last (list rows calls)))

(module+ test
  (require racket/string
           (only-in racket/file
                    file->lines))

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
  (define b (board))
  (add-point b p)

  (define calls
    (map string->number
         (string-split "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1" ",")))

  (define input #<<BRD
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
BRD
    )

  (define rows
    '((("22" "13" "17" "11" "0")
       ("8" "2" "23" "4" "24")
       ("21" "9" "14" "16" "7")
       ("6" "10" "3" "18" "5")
       ("1" "12" "20" "15" "19"))

      (("3" "15" "0" "2" "22")
       ("9" "18" "13" "17" "5")
       ("19" "8" "7" "25" "23")
       ("20" "11" "10" "24" "4")
       ("14" "21" "16" "12" "6"))

      (("14" "21" "17" "24" "4")
       ("10" "16" "15" "9" "19")
       ("18" "8" "23" "26" "20")
       ("22" "11" "13" "6" "5")
       ("2" "0" "12" "3" "7"))))

  ;; Create boards and run calls through them
  (play-bingo-last rows calls))

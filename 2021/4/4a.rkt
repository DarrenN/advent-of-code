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

(require (only-in racket/file
                  file->lines)
         (only-in racket/function
                  curry)
         (only-in racket/string
                  string-split)
         (only-in "../lib/utils.rkt"
                  group))

(struct point (x y v [called? #:auto #:mutable])
  #:auto-value #f
  #:transparent)

(struct board ([seen #:auto #:mutable] [points #:auto #:mutable])
  #:auto-value (hash)
  #:transparent)

;; hash keyed on point-y
(define (check-across ps)
  (define sorted
    (for/fold ([h (hash)])
              ([p (in-list ps)])
      (hash-update h (point-y p) (curry append (list p)) '())))
  (for/or ([(_ v) (in-hash sorted)])
    (>= (length v) 5)))

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

(define (check-board b)
  (define points (board-points b))
  (define ps (for/list ([k (in-list (hash-keys (board-seen b)))])
               (hash-ref points k)))
  (check-down-or-across ps))

(define (add-point b p)
  (set-board-points! b (hash-set (board-points b) (point-v p) p)))

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
                (sum-board b n)
                #f)
            #f))
      #f))

(define (parse-input input-list)
  (define calls (car input-list))
  (define boards
    (for/fold ([bs '()])
              ([b (in-list (group (cdr input-list) '() 6))])
      (cons (map string-split (cdr b)) bs)))
  (values calls boards))

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


(module+ main)

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
  (play-bingo rows calls))

#lang racket/base

(require racket/list
         racket/string)

;; https://adventofcode.com/2023/day/2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 2a

;; Scans a single line of input (a Game) and converts into a hash with max
;; values for each color cube. Example output:
;; #hash(("blue" . 6) ("green" . 2) ("id" . 1) ("red" . 4))
(define (scan-input l)
  ;; Extract cube values and store the max in a hash keyed on color
  (define (store-value buf cap)
    (let* ([vs (regexp-match #px"([\\d]*)([\\w]*)" buf)]
           [ct (string->number (cadr vs))]
           [key (caddr vs)])
      (hash-set cap key (max (hash-ref cap key 0) ct))))

  ;; Read the line by char and decide how to handle substrings based on
  ;; separators
  (for/fold ([buf ""]
             [cap (hash)]
             #:result (store-value buf cap))
            ([i (in-list (string->list l))])
    (cond
      [(equal? i #\:)
       (values
        ""
        (hash-set
         cap "id"
         (string->number (cadr (regexp-match #px"Game([\\d]*)" buf)))))]
      [(or (equal? i #\,) (equal? i #\;))
       (values "" (store-value buf cap))]
      [(or (equal? i #\space)) (values buf cap)]
      [else (values
             (apply string (append (string->list buf) `(,i))) cap)])))

;; Check each parsed game to see if it's a "possible game" and add it's id
;; to the sum
(define (find-winners ls r g b)
  (for/fold ([sum 0])
            ([l (in-list ls)])
    (let* ([game (scan-input l)]
           [id (hash-ref game "id")]
           [red (hash-ref game "red")]
           [green (hash-ref game "green")]
           [blue (hash-ref game "blue")])
      (if (and (<= red r) (<= green g) (<= blue b))
          (+ sum id)
          sum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 2b

(define (find-powers ls)
  (for/fold ([sum 0])
            ([l (in-list ls)])
    (let* ([game (scan-input l)]
           [id (hash-ref game "id")]
           [red (hash-ref game "red")]
           [green (hash-ref game "green")]
           [blue (hash-ref game "blue")])
      (+ sum (* red green blue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test

(module+ test
  (require rackunit
           racket/string
           (only-in racket/file file->lines))

  (define (prep-test-input s)
    (string-split s #px"\n"))

  (define test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  ;; 12 red cubes, 13 green cubes, and 14 blue cubes
  ;;(find-powers (prep-test-input test-input))

  (find-powers (file->lines "./input.txt"))

  )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2023 4b


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


  ;; 3a
  ;;(find-parts (parse-input (file->lines "./input.txt")))

  )

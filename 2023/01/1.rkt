#lang racket/base

(require racket/list)

;; https://adventofcode.com/2023/day/1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(define patterns
  '((#px"one" #\1)
    (#px"two" #\2)
    (#px"three" #\3)
    (#px"four" #\4)
    (#px"five" #\5)
    (#px"six" #\6)
    (#px"seven" #\7)
    (#px"eight" #\8)
    (#px"nine" #\9)
    (#px"[\\d]" "+")))

;; Use the positions from regexp-match-positions* to keep order
;; when building a new string with digits instead of words.
;; we also need to keep any digits that were in the original string.
;; output: 2--19---
(define (convert-string newst oldst idx rep)
  (for/fold ([acc newst])
            ([i (in-list idx)])
    (if (equal? rep "+")
        (string-set! acc (car i) (string-ref oldst (car i)))
        (string-set! acc (car i) rep))
    acc))

;; convert words to numbers maintaining order
(define (match-digits s ps)
  (for/fold ([acc (make-string (string-length s) #\-)])
            ([p (in-list ps)])
    (define ms (regexp-match-positions* (car p) s))
    (if (null? ms)
        acc
        (convert-string acc s ms (cadr p)))))

;;  29, 83, 13, 24, 42, 14, and 76 => 281
(define (process-lines lines)
  (for/list ([l lines])
    (match-digits l patterns)))

(define (sum-lines lines)
  (for/fold ([accum 0])
            ([l (in-list lines)])
    (let ([cs (filter char-numeric? (string->list l))])
      (+ accum (string->number (string (first cs) (last cs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test

(module+ test
  (require rackunit
           racket/list
           (only-in racket/file file->lines))

  (define test-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

  (define test-input2
    "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

  ;; (string-split test-input2 #px"\\s")

  (define (1b)
    (println (sum-lines (process-lines (file->lines "./input.txt")))))

  (define (1a)
    (define lines
      (file->lines "./input.txt"))
    (println (sum-lines lines)))

  (1a)
  (1b))

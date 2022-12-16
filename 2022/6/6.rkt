#lang racket/base

(require racket/list)

(define (is-marker? seen [size 4])
  (cond [(< (length seen) size) #f]
        [(check-duplicates seen) #f]
        [else #t]))

;; Keep a sliding window of size
(define (push seen c [size 4])
  (if (< (length seen) size)
      (append seen `(,c))
      (append (drop seen 1) `(,c))))

;; Size denotes marker or message (4 | 14)
(define (parse in [size 4])
  (define (loop in seen pos)
    (let* ([c (read-char in)]
           [s (push seen c size)])
      (cond [(eof-object? c) s]
            [(is-marker? s size) (add1 pos)]
            [else (loop in s (add1 pos))])))
  (loop in '() 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit)

  (define a (open-input-string "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
  (define b (open-input-string "bvwbjplbgvbhsrlpgdmjqwftvncz"))
  (define c (open-input-string "nppdvjthqldpwncqszvftbrmjlhg"))
  (define d (open-input-string "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
  (define e (open-input-string "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

  (check-eq? (parse a) 7)
  (check-eq? (parse b) 5)
  (check-eq? (parse c) 6)
  (check-eq? (parse d) 10)
  (check-eq? (parse e) 11)

  ;(call-with-input-file "./input.txt" parse)

  (define ma (open-input-string "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
  (define mb (open-input-string "bvwbjplbgvbhsrlpgdmjqwftvncz"))
  (define mc (open-input-string "nppdvjthqldpwncqszvftbrmjlhg"))
  (define md (open-input-string "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
  (define me (open-input-string "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

  (check-eq? (parse ma 14) 19)
  (check-eq? (parse mb 14) 23)
  (check-eq? (parse mc 14) 23)
  (check-eq? (parse md 14) 29)
  (check-eq? (parse me 14) 26)

  (call-with-input-file "./input.txt" (λ (i) (parse i 14))))

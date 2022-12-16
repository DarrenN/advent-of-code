#lang racket/base

(require racket/list)


(define (is-marker? seen)
  (cond [(< (length seen) 4) #f]
        [(check-duplicates seen) #f]
        [else #t]))

(define (push seen c)
  (if (< (length seen) 4)
      (append seen `(,c))
      (append (drop seen 1) `(,c))))

(define (parse in)
  (define (loop in seen pos)
    (let* ([c (read-char in)]
           [s (push seen c)])
      (cond [(eof-object? c) s]
            [(is-marker? s) (add1 pos)]
            [else (loop in s (add1 pos))])))
  (loop in '() 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit
           (only-in racket/file file->lines))

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
  
  ;(parse a)
  (call-with-input-file "./input.txt" parse)

  )

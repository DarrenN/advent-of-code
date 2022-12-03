#lang racket/base

(require racket/set
         (only-in racket/file
                  file->lines)
         (only-in racket/string
                  string-split
                  string-trim)
         (only-in "../lib/timed.rkt"
                  print-timed))

(define (get-input path)
  (map (λ (line)
         (let* ([fs (string-split line " | ")]
                [signal (string-split (car fs))]
                [digits (string-split (cadr fs))])
           (list signal digits)))
       (file->lines path)))

(define zero  '(0 1 2 4 5 6))
(define one   '(2 5))
(define two   '(0 2 3 4 6))
(define three '(0 2 3 5 6))
(define four  '(1 2 3 5))
(define five  '(0 1 3 5 6))
(define six   '(0 1 3 4 5 6))
(define seven '(0 2 5))
(define eight '(0 1 2 3 4 5 6))
(define nine  '(0 1 2 3 5 6))

(define wires->digit
  (hash (set 'a 'b 'c 'e 'f 'g) 0
        (set 'c 'f) 1
        (set 'a 'c 'd 'e 'g) 2
        (set 'a 'c 'd 'f 'g) 3
        (set 'b 'c 'd 'f) 4
        (set 'a 'b 'd 'f 'g) 5
        (set 'a 'b 'd 'e 'f 'g) 6
        (set 'a 'c 'f) 7
        (set 'a 'b 'c 'd 'e 'f 'g) 8
        (set 'a 'b 'c 'd 'f 'g) 9))

(define (count-digits input)
  (for/sum ([i (in-list input)])
    (for/sum ([digit (in-list (cadr i))])
      (case (string-length digit)
        [(2) 1]
        [(3) 1]
        [(4) 1]
        [(7) 1]
        [else 0]))))

(define (tally tbl sn char pos)
  (hash-set! tbl char pos)
  (set-add! sn char))

(define (analyze2 signal)
  (define table (make-hash))
  (for ([s (in-list signal)])
    (case (string-length s)
      [(2) (hash-set! )]
      [(3) (tally table seen (string-ref s 0) 0)
           (tally table seen (string-ref s 1) 2)
           (tally table seen (string-ref s 2) 5)]
      [(4) (tally table seen (string-ref s 0) 1)
           (tally table seen (string-ref s 1) 2)
           (tally table seen (string-ref s 2) 3)
           (tally table seen (string-ref s 3) 5)]
      [(6) ])))

(define (analyze signal)
  (define seen (mutable-set))
  (define table (make-hash))
  (for ([s (in-list signal)])
    (case (string-length s)
      [(2) (tally table seen (string-ref s 0) 2)
           (tally table seen (string-ref s 1) 5)]
      [(3) (tally table seen (string-ref s 0) 0)
           (tally table seen (string-ref s 1) 2)
           (tally table seen (string-ref s 2) 5)]
      [(4) (tally table seen (string-ref s 0) 1)
           (tally table seen (string-ref s 1) 2)
           (tally table seen (string-ref s 2) 3)
           (tally table seen (string-ref s 3) 5)]
      [(6) ]))
  (values table seen))

(module+ main
  (define input (get-input "./input.txt"))
  ; 8a
  (print-timed count-digits (list input)))

(module+ test
  (define xs '((("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab")
                ("cdfeb" "fcadb" "cdfeb" "cdbaf"))))
  (count-digits xs)
  (analyze '("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb"
                       "cagedb" "ab")))

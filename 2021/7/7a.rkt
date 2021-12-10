#lang racket/base

(require (only-in racket/file
                  file->lines)
         (only-in racket/string
                  string-split)
         (only-in "../lib/timed.rkt"
                  print-timed))

(define (get-input path)
  (map string->number (string-split (car (file->lines path)) ",")))

(define (find-crabs input)
  (define avg (round (/ (apply + input) (length input))))
  (car (sort (for/list ([i (in-range avg)])
              (list
               i (for/sum ([x (in-list input)])
                   (if (>= x i) (- x i) (- i x)))))
            #:key cadr <)))

(define (find-crabs2 input)
  (define (step n m)
    (for/sum ([n (in-range (add1 (- n m)))]) n))
  (car
   (sort (for/list ([i (in-range (length input))])
           (list
            i (for/sum ([x (in-list input)])
                (if (>= x i) (step x i) (step i x)))))
         #:key cadr <)))

(module+ main
  (define xs (get-input "./input.txt"))

  (print-timed find-crabs (list xs))
  (print-timed find-crabs2 (list xs)))

(module+ test

  (define xs '(16 1 2 0 4 2 7 1 2 14))
  (define avg (round (/ (apply + xs) (length xs))))
    (sort (for/list ([i (in-range avg)])
          (list
           i (for/sum ([x (in-list xs)])
               (if (>= x i) (- x i) (- i x)))))
        #:key cadr <)

    ;(get-input "./input.txt")
  (find-crabs2 xs)
  )

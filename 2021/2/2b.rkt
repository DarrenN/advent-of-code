#lang racket/base

(require (only-in racket/file
                  file->list)
         racket/match
         (only-in "../lib/timed.rkt"
                  print-timed)
         (only-in "../lib/utils.rkt"
                  group))

(define input (group (file->list "./input.txt") '() 2))

(define (parse-commands cmds)
  (define (unpack-pos p)
    (values (vector-ref p 0) (vector-ref p 1) (vector-ref p 2)))
  (define (forward p n)
    (let-values ([(x y v) (unpack-pos p)])
      (vector (+ n x) (+ y (* n v)) v)))
  (define (aim p n op)
    (let-values ([(x y v) (unpack-pos p)])
      (vector x y (apply op (list v n)))))

  (let ([pos (for/fold ([pos (vector 0 0 0)])
                       ([cmd (in-list cmds)])
               (match cmd
                 [(list 'forward n) (forward pos n)]
                 [(list 'down n) (aim pos n +)]
                 [(list 'up n) (aim pos n -)]))])
    (* (vector-ref pos 0) (vector-ref pos 1))))

(print-timed parse-commands `(,input))

(module+ test
  (require racket/match
           (only-in "../lib/utils.rkt"
                    group))
  (define input
    '(forward 5 down 5 forward 8 up 3 down 8 forward 2))

  (parse-commands (group input '() 2)))

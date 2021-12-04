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
  (let ([pos (for/fold ([pos '(0 . 0)])
                       ([cmd (in-list cmds)])
               (match cmd
                 [(list 'forward v) (cons (+ v (car pos)) (cdr pos))]
                 [(list 'down v) (cons (car pos) (+ (cdr pos) v))]
                 [(list 'up v) (cons (car pos) (- (cdr pos) v))]))])
    (* (car pos) (cdr pos))))

(print-timed parse-commands `(,input))

(module+ test
  (require racket/match
           (only-in "../lib/utils.rkt"
                    group))
  (define input
    '(forward 5 down 5 forward 8 up 3 down 8 forward 2))

  (parse-commands (group input '() 2)))

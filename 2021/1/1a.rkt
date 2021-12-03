#lang racket/base

(require
  (only-in racket/file file->list)
  "../lib/timed.rkt")

#|

Day 1a:
=======

count the number of times a depth measurement increases from the previous measurement.
(There is no measurement before the first measurement.)
In the example above, the changes are as follows:

199 (N/A - no previous measurement)
200 (increased)
208 (increased)
210 (increased)
200 (decreased)
207 (increased)
240 (increased)
269 (increased)
260 (decreased)
263 (increased)

In this example, there are 7 measurements that are larger than the previous
measurement.

|#

(define (count-depths depths)
  (define-values (_ increased-count)
    (for/fold ([seen 0]
               [ct 0])
              ([d (in-list depths)])
      (if (zero? seen)
          (values d ct)
          (if (> d seen)
              (values d (add1 ct))
              (values d ct)))))
  increased-count)

(define-values (result ms)
  (timed-apply count-depths (list (file->list "./input-1a.txt"))))

(printf "Number of increases: ~a~n" result)
(printf "Duration in ms: ~a~n" ms)

(module+ test
  (require rackunit
           (only-in racket/file file->list))
  (define depths '(199
                   200
                   208
                   210
                   200
                   207
                   240
                   269
                   260
                   263))
  (define-values (result dur)
    (timed-apply count-depths (list depths)))
  (check-equal? result 7))

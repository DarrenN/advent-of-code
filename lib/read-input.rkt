#lang racket/base

(require racket/port
         racket/string)

(provide read-input)

;; (-> string string)
(define (read-input path)
  (port->string (open-input-file path) #:close? #t))

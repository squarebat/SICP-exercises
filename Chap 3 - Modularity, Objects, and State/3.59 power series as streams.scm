#lang sicp
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;a
(define (integrate-series series)
  (stream-map / series integers))

;b
(define sine-series
  (cons-stream 1 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 0 (scale-stream -1 (integrate-series sine-series))))
#lang racket

;;------------------------------------------------------------------------------
;; Day 3
;;------------------------------------------------------------------------------

(define input-path "03.txt")

(define (get-input) ;=> '((1 0 1 0 1) (0 1 0 1 0) ...)
  (call-with-input-file
    input-path
    (λ (in)
      (for/list ([line (in-lines in)])
        (bitstr->bitlist line)))))

(define (bitstr->bitlist str) ;=> '(1 0 1 0 1)
  (for/list ([c (in-string str)])
    (if (eq? c #\0) 0 1)))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (for/fold ([rows 0]
             [tally #(0 0 0 0 0 0 0 0 0 0 0 0)]
             #:result (power-consumption tally rows))
            ([bits (in-list (get-input))])
    (values (add1 rows) (tally-bits tally bits))))

(define (tally-bits tally bits)
  (for/list ([t (in-list tally)]
             [b (in-list bits)])
    (+ t b)))

(define (gamma-rate bits rows)
  (for/fold ([gbits '()]
             #:result (string->number (list->string gbits) 2))
            ([b (in-list bits)])
    (if (> b (/ rows 2)) 1 0)))

(define (epsilon-rate bits rows)
  (for/list ([b (in-list bits)])
    (if (> b (/ rows 2)) 0 1)))

(define (power-consumption tally rows) 0)

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))

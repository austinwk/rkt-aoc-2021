#lang racket

;;------------------------------------------------------------------------------
;; Day 3
;;------------------------------------------------------------------------------

(define input-path "03.txt")

;=> '(#(#\0 #\0 #\1) ...)
(define (get-input)
  (call-with-input-file
    input-path
    (λ (in)
      (for/list ([line (in-lines in)])
        (list->vector (string->list line))))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (for/fold ([rows 0]
             [tally #(0 0 0 0 0 0 0 0 0 0 0 0)]
             #:result (power-consumption tally rows))
            ([bits (in-list (get-input))])
    (values (add1 rows) (tally-bits tally bits))))

(define (tally-bits tally bits) #())

(define (power-consumption tally rows) 0)

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))

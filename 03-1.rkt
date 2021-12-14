#lang racket

;;------------------------------------------------------------------------------
;; Day 3 Part 1
;;------------------------------------------------------------------------------

(define input-path "03.txt")

(define (solve-part-1) ;=> 2967914
  (for/fold ([rows 0]
             [tally '(0 0 0 0 0 0 0 0 0 0 0 0)]
             #:result (power-consumption tally rows))
            ([bits (in-list (get-input))])
    (values (add1 rows) (tally-bits tally bits))))

(define (get-input) ;=> list<list<number>>
  (call-with-input-file
    input-path
    (λ (in)
      (for/list ([line (in-lines in)])
        (bitstr->bitlist line)))))

(define (bitstr->bitlist str) ;=> list<number>
  (for/list ([c (in-string str)])
    (if (eq? c #\0) 0 1)))

(define (tally-bits tally bits) ;=> list<number>
  (for/list ([t (in-list tally)]
             [b (in-list bits)])
    (+ t b)))

(define (gamma-rate bits rows) ;=> number
  (for/fold ([gbstr ""]
             #:result (string->number gbstr 2))
            ([b (in-list bits)])
    (string-append gbstr (if (> b (/ rows 2)) "1" "0"))))

(define (epsilon-rate bits rows) ;=> number
  (for/fold ([ebstr ""]
             #:result (string->number ebstr 2))
            ([b (in-list bits)])
    (string-append ebstr (if (> b (/ rows 2)) "0" "1"))))

(define (power-consumption tally rows) ;=> number
  (* (gamma-rate tally rows) (epsilon-rate tally rows)))

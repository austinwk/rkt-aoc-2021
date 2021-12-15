#lang racket

;;------------------------------------------------------------------------------
;; Day 3 Part 2
;;------------------------------------------------------------------------------

(define input-path "03.txt")

(define (solve-part-2)
  (define input (get-input))
  (define oxy-bitvec (find-one input oxy-comparator))
  (define co2-bitvec (find-one input co2-comparator))
  (life-support-rating (bitvec->rating oxy-bitvec) (bitvec->rating oxy-bitvec)))

(define (get-input) ;=> list<vec<number>>
  (call-with-input-file
    input-path
    (λ (in)
      (for/list ([line (in-lines in)])
        (bitstr->bitvec line)))))

(define (bitstr->bitvec str) ;=> vec<number>
  (for/vector ([c (in-string str)])
    (if (eq? c #\0) 0 1)))

(define (bitvec->rating bitvec) ;=> number
  (for/fold ([bitstr ""]
             #:result (string->number bitstr 2))
            ([bit (in-vector bitvec)])
    (string-append bitstr (number->string bit))))

(define (find-one bits comparator)
  (for/fold ([candidates bits]
             #:result (car candidates))
            ([col (in-range 12)]
             #:break (= 1 (length candidates)))
    (filter-bits candidates col comparator)))

(define (filter-bits bits col comparator)
  (for/fold ([zeros '()]
             [ones '()]
             #:result (comparator zeros ones))
            ([bitvec (in-list bits)])
    (if (= 0 (vector-ref bitvec col))
        (values (cons bitvec zeros) ones)
        (values zeros (cons bitvec ones)))))

(define (oxy-comparator zeros ones) ;=> list<vec<number>>
    (define zero-len (length zeros))
    (define ones-len (length ones))
    (cond
      [(> zero-len ones-len) zeros]
      [(> zero-len ones-len) ones]
      [else ones]))

(define (co2-comparator zeros ones) ;=> list<vec<number>>
    (define zero-len (length zeros))
    (define ones-len (length ones))
    (cond
      [(> zero-len ones-len) ones]
      [(< zero-len ones-len) zeros]
      [else zeros]))

(define (life-support-rating oxy-gen-rtg co2-scrub-rtg) ;=> number
  oxy-gen-rtg * co2-scrub-rtg)

(displayln (solve-part-2))

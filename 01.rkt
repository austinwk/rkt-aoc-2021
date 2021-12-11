#lang racket

;;------------------------------------------------------------------------------
;; Day 1
;;------------------------------------------------------------------------------

(define input-path "01.txt")

(define (get-input)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 1288
  (for*/fold ([increases 0]
              [prev-depth +inf.0]
              #:result increases)
             ([depth (in-list (get-input))])
    (values (if (< prev-depth depth)
                 (add1 increases)
                 increases)
            depth)))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 1311
  (define input (get-input))
  (for/fold ([cnt 0]
             [prv +inf.0]
             #:result cnt)
            ([a (in-list (drop-right input 2))]
             [b (in-list (drop (drop-right input 1) 1))]
             [c (in-list (drop input 2))])
    (if (> (+ a b c) prv)
        (values (add1 cnt) (+ a b c))
        (values cnt (+ a b c)))))

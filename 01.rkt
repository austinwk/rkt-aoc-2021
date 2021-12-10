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

(displayln (solve-part-1))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))

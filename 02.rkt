#lang racket

;;------------------------------------------------------------------------------
;; Day 2
;;------------------------------------------------------------------------------

(define input-path "02.txt")

(define (get-input)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/list ([line (in-lines in)])
        (define command (string-split line " "))
        (define instruction (car command))
        (define units (string->number (cadr command)))
        (list instruction units)))))

(println (take (get-input) 5))
;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (error "unimplemented"))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))

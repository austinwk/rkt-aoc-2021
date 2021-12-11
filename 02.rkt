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

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 1714680
  (for/fold ([horiz 0]
             [depth 0]
             #:result (* horiz depth))
             ([command (in-list (get-input))])
    (define direction (first command))
    (define distance (second command))
    (match direction
      ["up" (values horiz (- depth distance))]
      ["down" (values horiz (+ depth distance))]
      ["forward" (values (+ horiz distance) depth)])))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))

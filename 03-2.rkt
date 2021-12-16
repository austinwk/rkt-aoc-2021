#lang racket

;;------------------------------------------------------------------------------
;; Day 3 Part 2
;;------------------------------------------------------------------------------

(define input-path "03.txt")
(define sample-input-path "03-sample.txt")

(define (parse-input file-path) ;=> list<vec<num>>
  (call-with-input-file
    file-path
    (lambda (file-port)
      (for/list ([line (in-lines file-port)])
        (list->vector (map (λ (c)
                             (if (eq? c #\0) 0 1))
                           (string->list line)))))))

(define (oxygen-rating data)
  (define num-cols (vector-length (car data)))
  (for/fold ([remaining data]
             #:result (bitvec->num (car remaining)))
            ([col-idx (in-range num-cols)])
             #:break (= 1 (length remaining))
    (filter-on-col remaining col-idx)))

(define (filter-on-col data col)
  (for/fold ([zeros '()]
             [ones '()]
             #:result (cond
                        [(> (length zeros) (length ones)) zeros]
                        [(< (length zeros) (length ones)) ones]
                        [else ones]))
            ([row (in-list data)])
    (if (= 0 (vector-ref row col))
        (values (cons row zeros) ones)
        (values zeros (cons row ones)))))

(define (bitvec->num bitvec)
  (for/fold ([str ""]
             #:result (string->number str 2))
            ([bit (in-vector bitvec)])
    (string-append str (number->string bit))))

(define (solve-part-2) 0)

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)
    (define normalized-input
      '(#(0 0 1 0 0)
        #(1 1 1 1 0)
        #(1 0 1 1 0)
        #(1 0 1 1 1)
        #(1 0 1 0 1)
        #(0 1 1 1 1)
        #(0 0 1 1 1)
        #(1 1 1 0 0)
        #(1 0 0 0 0)
        #(1 1 0 0 1)
        #(0 0 0 1 0)
        #(0 1 0 1 0)))

    (test-case "parse-input"
      (check-equal? (parse-input sample-input-path)
                    normalized-input))

    (test-case "bitvec->num"
      (check-eq? (bitvec->num #(0 1 1 0 1 1 0 1 0 1))
                 437))

    (test-case "filter-on-col"
      (check-equal? (filter-on-col normalized-input 0)
                    ; The procedure reverses the list during filtering
                    '(#(1 1 0 0 1)
                      #(1 0 0 0 0)
                      #(1 1 1 0 0)
                      #(1 0 1 0 1)
                      #(1 0 1 1 1)
                      #(1 0 1 1 0)
                      #(1 1 1 1 0))))

    (test-case "oxygen-rating"
      (check-eq? (oxygen-rating normalized-input) 23))
)

#lang racket

;;------------------------------------------------------------------------------
;; Day 3 Part 2
;;------------------------------------------------------------------------------

(define input-path "03.txt")
(define sample-input-path "03-sample.txt")



(define (solve)
  (life-rating (file->data input-path)))

(define (file->data path)
  (call-with-input-file
    path
    (λ (file-port)
      (for/list ([line (in-lines file-port)])
        (list->vector (map (λ (c)
                             (if (eq? c #\0) 0 1))
                           (string->list line)))))))



(define (life-rating data)
  (* (oxy-rating data) (co2-rating data)))

(define (oxy-rating data)
  (rating data oxy-comparator))

(define (co2-rating data)
  (rating data co2-comparator))

(define (oxy-comparator zeros ones)
  (cond [(> (length zeros) (length ones)) zeros]
        [(< (length zeros) (length ones)) ones]
        [else ones]))

(define (co2-comparator zeros ones)
  (cond [(> (length zeros) (length ones)) ones]
        [(< (length zeros) (length ones)) zeros]
        [else zeros]))



(define (rating data comparator)
  (define num-cols (vector-length (car data)))
  (for/fold ([remaining data]
             #:result (bitvec->num (car remaining)))
            ([col-idx (in-range num-cols)])
             #:break (= 1 (length remaining))
    (filter-on-col remaining col-idx comparator)))

(define (filter-on-col data col comparator)
  (for/fold ([zeros '()]
             [ones '()]
             #:result (comparator zeros ones))
            ([row (in-list data)])
    (if (= 0 (vector-ref row col))
        (values (cons row zeros) ones)
        (values zeros (cons row ones)))))

(define (bitvec->num bitvec)
  (for/fold ([str ""]
             #:result (string->number str 2))
            ([bit (in-vector bitvec)])
    (string-append str (number->string bit))))



;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)
    (define sample-data
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
      (check-equal? (file->data sample-input-path) sample-data))

    (test-case "bitvec->num"
      (check-eq? (bitvec->num #(0 1 1 0 1 1 0 1 0 1)) 437))

    (test-case "oxy-rating"
      (check-eq? (oxy-rating sample-data) 23))

    (test-case "co2-rating"
      (check-eq? (co2-rating sample-data) 10))

    (test-case "life-rating"
      (check-eq? (life-rating sample-data) 230))

    (test-case "solve"
      (check-eq? (solve) 7041258)))

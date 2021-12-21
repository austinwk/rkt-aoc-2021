#lang racket

;;------------------------------------------------------------------------------
;; Day 4 Part 1
;;------------------------------------------------------------------------------

(define input-path "04.txt")
(define sample-input-path "04-sample.txt")



(define (parse-data path)
  (define chunks (string-split (file->string path) "\n\n"))
  (define nums (map string->number (string-split (first chunks) ",")))
  (define boards (for/list ([board-str (in-list (rest chunks))])
                   (string->board board-str)))
  (values nums boards))

(define (string->board str)
  (for/vector ([row (in-list (string-split str "\n"))])
    (list->vector (map string->number
                       (string-split row)))))



(define (seek-n-dob! board num)
  (define dobbed? #f)
  (for ([row (in-vector board)])
    (for ([i (in-range (vector-length row))])
      (when (eq? num (vector-ref row i))
        (vector-set! row i #t)
        (set! dobbed? #t))))
  (values board dobbed?))

(define (bingo? board)
  (or (bingo-in-rows? board)
      (bingo-in-cols? board)))

(define (bingo-in-rows? board)
  (for/or ([row (in-vector board)])
    (for/and ([n (in-vector row)])
      (eq? #t n))))

(define (bingo-in-cols? board)
  (for/or ([i (in-range (vector-length (vector-ref board 0)))])
    (for/and ([row (in-vector board)])
      (eq? #t (vector-ref row i)))))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (define nums '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))
  (define boards '(#(#(22 13 17 11  0)
                     #( 8  2 23  4 24)
                     #(21  9 14 16  7)
                     #( 6 10  3 18  5)
                     #( 1 12 20 15 19))

                   #(#( 3 15  0  2 22)
                     #( 9 18 13 17  5)
                     #(19  8  7 25 23)
                     #(20 11 10 24  4)
                     #(14 21 16 12  6))

                   #(#(14 21 17 24  4)
                     #(10 16 15  9 19)
                     #(18  8 23 26 20)
                     #(22 11 13  6  5)
                     #( 2  0 12  3  7))))

  (test-case "parse-data"
             (define-values (tnums tboards) (parse-data sample-input-path))
             (check-equal? tnums nums
                           (check-equal? tboards boards)))

  (test-case "string->board"
             (check-equal? (string->board "14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
                           #(#(14 21 17 24  4)
                             #(10 16 15  9 19)
                             #(18  8 23 26 20)
                             #(22 11 13  6  5)
                             #( 2  0 12  3  7))))

  (test-case "bingo-in-rows?"
             (check-equal? (bingo-in-rows? #(#( 3 15  0  2 22)
                                             #( 9 18 #t 17  5)
                                             #(#t #t #t 25 #t)
                                             #(#t 11 10 #t  4)
                                             #(#t #t #t #t #t)))
                           #t)
             (check-equal? (bingo-in-rows? #(#( 3 15  0  2 22)
                                             #( 9 18 #t 17  5)
                                             #(#t #t #t 25 #t)
                                             #(#t 11 10 #t  4)
                                             #(#t 13 #t #t #t)))
                           #f))

  (test-case "bingo-in-cols?"
    (check-equal? (bingo-in-cols? #(#(14 #t 17 #t  4)
                                    #(10 #t 15 #t 19)
                                    #(18  8 23 #t 20)
                                    #(#t #t #t #t #t)
                                    #( 2 #t 12 #t  7)))
                  #t)
    (check-equal? (bingo-in-cols? #(#(14 #t 17 #t  4)
                                    #(10 #t 15 12 19)
                                    #(18  8 23 #t 20)
                                    #(#t #t #t #t #t)
                                    #( 2 #t 12 #t  7)))
                  #f))

  (test-case "seek-n-dob"
    (define t-board (vector (vector 22 13 17 11  0) ; #(...) is immutable
                            (vector  8  2 23  4 24)
                            (vector 21  9 14 16  7)
                            (vector  6 10  3 18  5)
                            (vector  1 12 20 15 19)))
    (define-values (n-board n-dobbed?) (seek-n-dob! t-board 18))
    (check-equal? n-board #(#(22 13 17 11  0)
                            #( 8  2 23  4 24)
                            #(21  9 14 16  7)
                            #( 6 10  3 #t  5)
                            #( 1 12 20 15 19)))
    (check-eq? n-dobbed? #t))
)

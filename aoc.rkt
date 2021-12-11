#lang racket

; Thanks, Yanying Wang! https://docs.racket-lang.org/http-client/index.html
(require http-client)

(define (get-session-id)
  (call-with-input-file ".session-id" (lambda (port) (read-line port))))

(define (get-input session-id day)
  (define response (http-get "https://adventofcode.com"
                             #:path (format "2021/day/~a/input" day)
                             #:headers (hasheq 'Cookie (format "session=~a" session-id))))
  (http-response-body response))

(define (write-input day data)
  (call-with-output-file (string-append (~a day
                                            #:min-width 2
                                            #:align 'right
                                            #:left-pad-string "0")
                                        ".txt")
                         (lambda (port) (display data port))))

(module+ main
  (define day (vector-ref (current-command-line-arguments) 0))
  (write-input day (get-input (get-session-id) day)))

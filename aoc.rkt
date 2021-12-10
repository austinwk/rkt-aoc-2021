#lang racket

(require net/http-client)

(define (get-session-id)
  (call-with-input-file ".session-id"
                             (lambda (port)
                               (read-line port))))

(define (get-input day)
  (let-values ([(status headers in)
                (http-sendrecv "adventofcode.com"
                               (format "/2021/day/~a/input" day)
                               #:ssl? #t
                               #:headers (list (format "Cookie:	session=~a" (get-session-id))))])
    (port->string in)))

(define (write-input day data)
  (call-with-output-file (string-append (~a day
                                            #:min-width 2
                                            #:align 'right
                                            #:left-pad-string "0")
                                        ".txt")
                           (lambda (port) (display data port))))

(module+ main
  (let ([day (vector-ref (current-command-line-arguments) 0)])
  (write-input day (get-input day))))

; (define (write-template day)
;   (call-with-output-file (string-append (~a day
;                                             #:min-width 2
;                                             #:align 'right
;                                             #:left-pad-string "0")
;                                         ".txt")))

; GET puzzle input
; ----------------
; 
; GET /2021/day/1/input HTTP/2
; Host: adventofcode.com
; User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:95.0) Gecko/20100101 Firefox/95.0
; Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8
; Accept-Language: en-US,en;q=0.5
; Accept-Encoding: gzip, deflate, br
; Referer: https://adventofcode.com/2021/day/1
; Connection: keep-alive
; Cookie: session=nope
; Upgrade-Insecure-Requests: 1
; Sec-Fetch-Dest: document
; Sec-Fetch-Mode: navigate
; Sec-Fetch-Site: same-origin
; DNT: 1
; Sec-GPC: 1
; Cache-Control: max-age=0
; TE: trailers

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

; POST answer

; Request
;
; POST /2020/day/11/answer HTTP/2
; Host: adventofcode.com
; User-Agent: {{browser}}
; Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
; Accept-Language: en-US,en;q=0.5
; Accept-Encoding: gzip, deflate, br
; Content-Type: application/x-www-form-urlencoded
; Content-Length: 28
; Origin: https://adventofcode.com
; Connection: keep-alive
; Referer: https://adventofcode.com/2020/day/11
; Cookie: session={{session-id}}; _ga={{id}}
; Upgrade-Insecure-Requests: 1
; DNT: 1
; Sec-GPC: 1
;
; level=1&answer=2024782584832

; Response
;
; HTTP/2 200 OK
; date: Sat, 22 May 2021 23:43:54 GMT
; content-type: text/html
; content-length: 1970
; server: Apache
; server-ip: 172.31.61.174
; vary: Accept-Encoding
; content-encoding: gzip
; strict-transport-security: max-age=300
; X-Firefox-Spdy: h2
;
; ...
; <main>
; <article><p>That's not the right answer; your answer is too high.  If you're stuck, make sure you're using the full input data; there are also some general tips on the <a href="/2020/about">about page</a>, or you can ask for hints on the <a href="https://www.reddit.com/r/adventofcode/" target="_blank">subreddit</a>.  Please wait one minute before trying again. (You guessed <span style="white-space:nowrap;"><code>2024782584832</code>.)</span> <a href="/2020/day/11">[Return to Day 11]</a></p></article>
; </main>
; ...

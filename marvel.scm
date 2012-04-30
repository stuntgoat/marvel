#lang racket

(define (run port)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
		(define socket (tcp-listen port 5 #t))
		(define (loop-listener)
		  (accept-loop socket)
		  (loop-listener))
		(thread loop-listener))
  (lambda () 
    (custodian-shutdown-all main-cust)))

(define (accept-loop socket)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
		(define-values (in-port out-port) (tcp-accept socket))
		(thread 
		 (lambda () 
		   (let* ((request (read-total-request in-port ""))
			  (request-path (begin 
					  (display request)
					  (acquire-GET-path request))))
		     (handle-request out-port request-path)
		     (close-input-port in-port)
		     (close-output-port out-port)))))
  (thread 
   (lambda ()
     (sleep 10)
     (custodian-shutdown-all cust))))

(define (acquire-GET-path request-string)
  (let* ((slash-index (car (car (regexp-match-positions* #rx"/" request-string))))
   	 (space-index (car (car (regexp-match-positions* #rx" " request-string slash-index)))))
    (substring request-string slash-index space-index)))

(define (newline? string)
  (cond ((eof-object? string) #t)
	((string=? "\n" string) #t)
	((string=? "\r" string) #t)
	((string=? "\r\n" string) #t)
	((string=? "\n\r" string) #t)
	(else #f)))

;; TODO: do not pass in string 
(define (read-total-request in-port response)
  (let ((line (read-line in-port)))
    (if (newline? line)
	(begin (display response)
	       response)
	(read-total-request in-port (string-append response line)))))

(define (handle-request client request-path)
  (cond ((string=? "/" request-path)
	 (if (file-exists? "index.html")
	     (write-response client "index.html")
	     (begin 
	       (display  "HTTP/1.0 200 OK\n\nWelcome to Marvel Web Server" client)
	       (display  "HTTP/1.0 200 OK Welcome to Marvel Web Server\n"))))
	((not (file-exists? (substring request-path 1 (string-length request-path))))
	 (begin 
	   (display (string-append "HTTP/1.0 404 WAT\n\n \"" request-path "\" not found") client)
	   (display (string-append "HTTP/1.0 404 " request-path "\n"))))
	(else (write-response client request-path))))

(define (write-response c request-path)
  (begin 
    (display "HTTP/1.0 200 OK\n\n" c)
    (display (string-append "HTTP/1.0 200 OK " request-path "\n"))
    (send-file c request-path)))

(define (send-file client filename)
  (let ((file (open-input-file (substring filename 1 (string-length filename)))))
    (let char-read-loop ((ch (read-char file)))
      (if (eof-object? ch)
	  '()
	  (begin
	    (display ch client)
	    (char-read-loop (read-char file)))))))

#lang racket

(require racket/tcp
         "Infrabel.rkt") 

(define (start-server port)
  (define listener (tcp-listen port))
  (printf "Infrabel server started on port ~a\n" port)
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () (handle-client in out)))
    (loop)))

(define (handle-client in out)
  (define (read-message in)
    (let ((msg (port->string in)))
      (string-trim msg)))

  (define (write-message out msg)
    (fprintf out "~a\n" msg)
    (flush-output out))

  (let loop ()
    (define request (read-message in))
    (unless (eof-object? request)
      (define response (process-request request))
      (write-message out response)
      (loop))))

(define (process-request request)
  (define request-parts (string-split request))
  (match (car request-parts)
    ["train" (apply-function-to-arguments message-to-infrabel-train (cadr request-parts) (caddr request-parts))]
    ["crossing" (apply-function-to-arguments message-to-infrabel-crossing (cadr request-parts) (caddr request-parts))]
    ["lights" (apply-function-to-arguments message-to-infrabel-lights (cadr request-parts) (caddr request-parts))]
    ["switches" (apply-function-to-arguments message-to-infrabel-switches (cadr request-parts) (caddr request-parts))]
    ["detectionb" (apply-function-to-arguments message-to-infrabel-detectionb (cadr request-parts) (cdr (cddr request-parts)))]))

(start-server 4000)  ;; Start the server on port 4000

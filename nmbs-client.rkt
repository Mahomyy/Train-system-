#lang racket

(require racket/tcp)

(define (connect-to-server host port)
  (define-values (in out) (tcp-connect host port))
  (values in out))

(define (send-request in out request)
  (fprintf out "~a\n" request)
  (flush-output out)
  (define response (port->string in))
  (string-trim response))

(define (message-to-infrabel-train message args-list)
  (define-values (in out) (connect-to-server "localhost" 4000))
  (define request (string-append "train " message " " args-list))
  (send-request in out request))

(define (message-to-infrabel-crossing message arg)
  (define-values (in out) (connect-to-server "localhost" 4000))
  (define request (string-append "crossing " message " " arg))
  (send-request in out request))

(define (message-to-infrabel-lights message arg)
  (define-values (in out) (connect-to-server "localhost" 4000))
  (define request (string-append "lights " message " " arg))
  (send-request in out request))

(define (message-to-infrabel-switches message arg)
  (define-values (in out) (connect-to-server "localhost" 4000))
  (define request (string-append "switches " message " " arg))
  (send-request in out request))

(define (message-to-infrabel-detectionb message . args)
  (define-values (in out) (connect-to-server "localhost" 4000))
  (define request (string-append "detectionb " message " " (apply string-append args)))
  (send-request in out request))

#lang racket

(define (atom? x)
  (and (not (null? x))
       (nor (pair? x))))

(define (not-atom? x)
  (not (atom? x)))

(define (lat? l)
  (cond
    [(null? l) #t]
    [(not-atom? (first l)) #f]
    [else (lat? (rest l))]
    ))

; Verifica se x é membro da lista l
(define (member? x l)
  (cond
    [(null? l) #f]
    [(eqv? x (first l)) #t]
    [else (member? x (rest l))]
    ))

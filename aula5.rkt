#lang racket

(define lg1 '(((ra a) b d a) (fd s (www b)) (d (a))))

;verifica se o atom está contido em listas internas
(define (membro*? a l)
  (cond
    [(null? l) #f]
    [(list? (first l))
            (or (membro*? a (first l))
                (membro*? a (rest l)))]
    [(eqv? a (first l)) #t]
    [else (membro*? a (rest l))])
)

; não funciona
(define (remova-membro*? a l)
  (cond
    [(null? l) empty]
    [(list? (first l))
            (or
                (cons (remova-membro*? a (first l)) (rest l))
                (append (first l) (remova-membro*? a (rest l))))]
    [(eqv? a (first l)) (rest l)]
    [else (cons (first l) (remova-membro*? a (rest l)))])
)

; Substitui o por n, em todos os níveis
(define (substitui* o n l)
    (cond
        [(null? l) empty]
        [(list? (first l))
            (cons (substitui* o n (first l))
                (substitui* o n (rest l)))]
        [(eqv? o (first l))
            (cons n (rest l)) ]
        [else
            (cons (first l) (substitui* o n (rest l)))]
))

(define (inverte* ln)
  (inverte-aux* ln '())
  )

(define (inverte-aux* ln la)
  (cond
    [(null? ln) la]
    [(list? (first ln))
         (inverte-aux*
             (rest ln)
             (cons (inverte-aux* (first ln) '()) la))]
    [else
         (inverte-aux* (rest ln) (cons (first ln) la))])
  )
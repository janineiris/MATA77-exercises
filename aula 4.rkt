#lang racket

; Soma números de uma lista
(define (soma-numeros ln)
  (cond
    [(null? ln) 0]
    [( + (soma-numeros (rest ln)) (first ln))]
    ))

; Soma com acumulador
(define (soma-numeros-aux ln a)
  (if (null? ln)
    a
    (soma-numeros-aux (rest ln) (+ (first ln) a))
    ))

; Versão 2 de soma números usando acumulador
(define (soma-numeros-v2 ln)
  (soma-numeros-aux ln 0))

(define l4 '(2 5 1 3))

; não é recursiva de cauda, é mais simples, equivalente a soma-numeros 
(foldr / 1 l4)
(/ 2 (/ 5 (/ 1 (/ 3 1))))

; é recursiva de cauda, usa acumulador, equivalente a soma-numeros-v2, é de mais alta ordem
(foldl / 1 l4)
(/ 3 (/ 1 (/ 5 (/ 2 1))))

; Remove v da lista l, não é recursiva de cauda
(define (remova a l)
  (cond
    [(null? l) empty]
    [(eqv? a (first l)) (rest l)]
    [else (cons (first l) (remova a (rest l)))]
  ))

(define (remova-todos a l)
  (cond
    [(null? l) empty]
    [(eqv? a (first l)) (remova-todos a (rest l))]
    [else (cons (first l) (remova-todos a (rest l)))]
  ))

(define (substitui-todos a n l)
  (cond
    [(null? l) empty]
    [(eqv? a (first l)) (cons n (substitui-todos a n (rest l)))]
    [else (cons (first l) (substitui-todos a n (rest l)))]
  ))
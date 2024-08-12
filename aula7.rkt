#lang racket

;(define (soma . numeros)

(define (monta-lista . elementos) elementos)

(define (soma . numeros)
  (if (null? numeros)
      0
      (+ (first numeros) (apply soma (rest numeros))))
  )

(define (soma2 . numeros)
  (soma-aux numeros))

(define (soma-aux ln)
  (if (null? ln)
      0
      (+ (first ln) (soma-aux (rest ln)))
      ))

(define (intercala-m n . elementos)
  (if (or (< n 1) (null? elementos))
    empty
    (cons (first elementos)
          (apply intercala-m
                 (- n 1)
                       (append (rest elementos)
                               (list (first elementos))))))
  )

(define (intercala-m2 n . elementos)
  (if (or (< n 1) (null? elementos))
    empty
    (cons (first elementos)
          (apply intercala-m2
                 (cons (- n 1)
                       (append (rest elementos)
                               (list (first elementos)))))))
  )

(define (mapeia f l)
  (if (null? l)
      empty
      (cons (f (first l))
            (mapeia f (rest l)))))
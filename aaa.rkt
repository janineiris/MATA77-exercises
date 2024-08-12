#lang racket

(define novo-fat
  ;; embala a função recrusiva usando(lambda (r) ...)
  (lambda (r)
    (lambda (n)
      (if (= n 0)
          1
          ;; substitui a chamada recursiva, novo-fat, por (r r)
          (* n ((r r) (- n 1)) )))))

(define aaa
  (lambda (r n)
      (if (= n 0)
          1
          ;; substitui a chamada recursiva, novo-fat, por (r r)
          (* n ((r r) (- n 1)) ))))

(define u
  (lambda (f) (f f)))

(define soma-numeros2
  (lambda (f)
    (lambda (l)
      (if (null? l)
          0
          (+ (first l) ((f f) (rest l)))))))
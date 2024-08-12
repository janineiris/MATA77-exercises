#lang racket

(define l1 '(a b c d e))
(define l2 '(a b (c d) e))
(define l3 '(2 2 3 3))
(define l4 '(2 5 1 3))
(define l5 '(1 2 3 4))
(define l6 '(a c b c d c e))
(define lg1 '(((ra a) b d a) (fd s (www b)) (d (a))))


(define (f1) #f)
(define (f2) #f)
(define (f3) "Manoel")
(define (f4) #f)

(define (atom? x)
  (and (not (null? x))
       (nor (pair? x))))

(define atom-v2?
  (lambda (x)
    (and (not (null? x))
         (nor (pair? x)))))

(define (lista-atomos? l)
  (cond
    [(null? l) #t]
    [(not (atom? (first l))) #f]
    [else (lista-atomos? (rest l))]
    ))

(define (membro? a l)
  (cond
    [(null? l) #f]
    [(eqv? a (first l)) #t]
    [else (membro? a (rest l))]
    ))

(define (soma-numeros ln)
  (if (null? ln)
      0
      (+ (first ln)
         (soma-numeros (rest ln)))
      ))

(define (soma-numeros-v2 ln)
  (soma-numeros-aux ln 0)
  )

(define (soma-numeros-aux ln a)
  (if (null? ln)
      a
      (soma-numeros-aux (rest ln) (+ (first ln) a))
      ))

(define (inverte ln)
  (inverte-aux ln '())
  )

(define (inverte-aux ln a)
  (if (null? ln)
      a
      (inverte-aux (rest ln) (cons (first ln) a))
      ))

(define (inverte-r ln)
  (if (null? ln)
      '()
      (cons (first ln)
            (inverte-r (rest ln)))
      ))

(define (remova x l)
  (cond
    [(null? l) empty]
    [(eqv? x (first l)) (rest l)]
    [else (cons (first l) (remova x (rest l)))]
    ))

(define (remova-todos x l)
  (cond
    [(null? l) empty]
    [(eqv? x (first l)) (remova-todos x (rest l))]
    [else (cons (first l) (remova-todos x (rest l)))]
    ))


(define (substitui o n l)
  (cond
    [(null? l) empty]
    [(eqv? o (first l))
     (cons n (substitui o n (rest l))) ]
    [else
     (cons (first l) (substitui o n (rest l)))]
    ))


(define (remova-rc a l)
  (remova-rc-aux a l '())
  )

(define (remova-rc-aux a l la)
  (cond
    [(null? l) empty]
    [(eqv? a (first l)) (append la (rest l))]
    [else
     (remova-rc-aux a (rest l) (cons (first l) la))]
    ))

;; Aula 26/08/2021

; Testa se a é membro de uma lista genérica
(define (membro*? a lg)
  (cond
    [(null? lg) #f]
    [(list? (first lg))
     (or (membro*? a (first lg))
         (membro*? a (rest lg)))]
    [(eqv? a (first lg)) #t]
    [else (membro*? a (rest lg))]
    ))


; Remove a todas as ocorrências de "a"
; data lista genérica
(define (remova* a lg)
  (cond
    [(null? lg) empty]
    [(list? (first lg))
     (cons (remova* a (first lg))
           (remova* a (rest lg)))]
    [(eqv? a (first lg))
     (remova* a (rest lg))]
    [else
     (cons (first lg)
           (remova* a (rest lg)))]
    ))

; subsitui todas as ocorrências do átomo old
; pelo átomo new
(define (substitui* o n lg)
  (cond
    [(null? lg) empty]
    [(list? (first lg))
     (cons (substitui* o n (first lg))
           (substitui* o n (rest lg)))]
    [(eqv? o (first lg))
     (cons n (substitui* o n (rest lg))) ]
    [else
     (cons (first lg) (substitui* o n (rest lg)))]
    ))



(define (inverte* ln)
  (inverte-aux* ln '())
  )

(define (inverte-aux* ln la)
  (cond
    [(null? ln) la]
    [(list? (first ln))
     (inverte-aux* (rest ln) (cons (inverte-aux* (first ln) '()) la))]
    ;(inverte-aux* (rest ln) (cons (inverte* (first ln)) la))]
    [else
     (inverte-aux* (rest ln) (cons (first ln) la))]
    ))

;; Aula 31/08/2021

#|

; errada por causa da definição recursiva

(let* [(is-even?  (lambda(n)
                    (or (zero? n)
                        (is-odd? (sub1 n)))))
       (is-odd? (lambda(n)
                  (and (not (zero? n))
                       (is-even? (sub1 n)))))]
  (is-odd? 3))


; correta

(letrec [(is-even?  (lambda(n)
                      (or (zero? n)
                          (is-odd? (sub1 n)))))
         (is-odd? (lambda(n)
                    (and (not (zero? n))
                         (is-even? (sub1 n)))))]
  (is-odd? 3))

|#

; Remove a todas as ocorrências de "a"
; data lista genérica
(define (remova-o* a lg)
  (cond
    [(null? lg) empty]
    [(list? (first lg))
     (cons (remova-o* a (first lg))
           (remova-o* a (rest lg)))]
    [(eqv? a (first lg))
     (remova-o* a (rest lg))]
    [else
     (cons (first lg)
           (remova-o* a (rest lg)))]
    ))

(define (remova-1* a lg)
  (rest (remova-b1* a lg)))

(define (remova-b1* a lg)
   (cond
    [(null? lg) (cons #f empty)]
    [(list? (first lg))
       (let [(resposta (remova-b1* a (first lg)))]
         (if (first resposta)
             (cons #t
                   (cons (rest resposta)
                         (rest lg)))
             (let [(resposta2 (remova-b1* a (rest lg)))]
               (cons (first resposta2)
                     (cons (first lg)
                           (rest resposta2))))))]
     [(eqv? a (first lg))
         (cons #t (rest lg))]
     [else
         (let [(resposta2 (remova-b1* a (rest lg)))]
           (cons (first resposta2)
                 (cons (first lg)
                       (rest resposta2))))]
     ))
        

(define (simples [a 0] [b 0] [c 0])
  (list a b c))

; inverte original
(define (inverte-o ln)
  (inverte-aux-o ln '())
  )

(define (inverte-aux-o ln la)
  (if (null? ln)
      la
      (inverte-aux-o (rest ln) (cons (first ln) la))
      ))


; inverte 2
(define (inverte2 ln [la '()])
  (if (null? ln)
      la
      (inverte2 (rest ln) (cons (first ln) la))
      ))






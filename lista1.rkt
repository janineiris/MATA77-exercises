#lang racket

#|
Questão 1
Concatenar l1 e l2
|#
(define (concatenar1 l1 l2)
  (append l1 l2))

#|
; Questão 1 - Solução Professor

(define (concatenar1 l1 l2)
  (if (null? l1)
      l2
      (cons (first l1) (concatenar1 (rest l1) l2))
      ))
|#

#|
Questão 2
Concatenar l2 e l1
|#
(define (concatenarInv l1 l2)
  (append l2 l1))

#|
Questão 3
Concatenar átomos de l1
|#
(define (concatenar2 l1)
  (concatenar2-aux l1 empty))

(define (concatenar2-aux l1 l2)
  (cond
    [(null? l1) l2]
    [(list? (first l1))
     (concatenar2-aux (rest l1) (concatenar2-aux (first l1) l2))]
    [else
     (concatenar2-aux (rest l1) (append l2 (list (first l1))))]
    ))

#|
; Questão 3 - Solução professor
|#
(define (concatenar2-v2 ll)
  (if (null? ll)
      empty
      (concatenar1 (first ll) (concatenar2 (rest ll)))))

#|
Questão 3-v
Concatenar N listas
|#
(define (concatenar3 . l)
  (concatenar2 l))

#|
Questão 4
Juntar duas listas, intercalando seus elementos
|#
(define (juntar l1 l2)
  (juntar-aux l1 l2 empty))

(define (juntar-aux l1 l2 acc)
  (cond
    [(null? l1) (append acc l2)]
    [(null? l2) (append acc l1)]
    [else
     (juntar-aux
      (rest l1)
      (rest l2)
      (append acc (list (first l1) (first l2)))
      )]
    ))

#|
Questão 5
Adicionar um elemento ao final de uma lista
|#
(define (adicionarFinal e l)
  (append l (list e)))

#|
Questão 6
Inverter uma lista
|#
(define (inverter L)
  (if (null? L) empty
    (append (inverter (rest L)) (list (first L)))))

#|
Questão 7
Cria uma lista de tamanho `n`, intercalada com os elementos e1 e e2
|#
(define (intercala n e1 e2)
  (intercala-aux n e1 e2 empty))

(define (intercala-aux n e1 e2 acc)
  (cond
    [(<= n 0) acc]
    [(= n 1) (append acc (list e1))]
    [else (intercala-aux (- n 2) e1 e2 (append acc (list e1 e2)))]
    ))

#|
Questão 7-v
Cria uma lista de tamanho `n`, com `m` elementos intercalados
  e
Questão 8
Cria uma lista de tamanho `n`, 
|#
(define (intercala2 n . e)
  (intercala2-aux n e e empty))

(define (intercala2-aux n eOg eCur acc)
  (if
    (<= n 0) acc
    (if
     (null? eCur) (intercala2-aux (- n 1) eOg (rest eOg) (append acc (list (first eOg))))
     (intercala2-aux (- n 1) eOg (rest eCur) (append acc (list (first eCur))))
     )))

#|
Questão 9
Cria uma lista de pares cujo primeiro elemento é `e` e o segundo é membro de `l`
|#
(define (parear e l)
  (parear-aux e l empty))

(define (parear-aux e l acc)
  (if
   (null? l) acc
   (parear-aux e (rest l) (append acc (list (cons e (first l)))))
   ))

#|
Questão 10
Criar uma lista de lista 
|#
(define (pares l)
  (pares-aux l (rest l) empty))

(define (pares-aux l a acc)
  (if
    (null? a)
     (if (null? (rest l))
         acc
         (pares-aux (rest l) (rest (rest l)) acc))
     (pares-aux l (rest a) (append acc (list (cons (first l) (first a)))))
    ))

#|
Questão 11 Difícil demais...
|#

#|
Questão 12
Informa se uma lista tem elementos repetidos
|#
(define (conjunto? l)
  (if
   (null? l) #t
   (conjunto-aux (first l) (rest l) (rest l))
   ))

(define (conjunto-aux el l og)
  (cond
    [(or (null? og) (null? (rest og))) #t]
    [(null? l) (conjunto-aux (first og) (rest og) (rest og))]
    [(eqv? el (first l)) #f]
    [else (conjunto-aux el (rest l) og)]))

#|
Questão 13
Testa se `l1` é prefixo de `l2`
|#
(define (prefixo? l1 l2)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(eqv? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
    [else #f]))

#|
Questão 14
Testa se `l1` é subsequência de `l2`
|#
(define (subsequencia? l1 l2)
  (subsequencia-aux l1 l2 l1))

(define (subsequencia-aux l1 l2 l1og)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(eqv? (first l1) (first l2)) (subsequencia-aux (rest l1) (rest l2) l1og)]
    [else (subsequencia-aux l1og (rest l2) l1og)]))

#|
Questão 15
Confere se duas listas são iguais
|#
(define (iguais-lg? lg1 lg2)
  (cond
    [(and (null? lg1) (null? lg2)) #t]
    [(xor (null? lg1) (null? lg2)) #f]
    [(eqv? (first lg1) (first lg2)) (iguais-lg? (rest lg1) (rest lg2))]
    [else (and (list? (first lg1))
               (list? (first lg2))
               (iguais-lg? (first lg1) (first lg2))
               (iguais-lg? (rest lg1) (rest lg2)))]
    ))

#|
Questão 16
Reescreva `remove-todos` a realizar chamada recursiva anonima e
remover todos os atomos `a` de `l`

(define (remove-todos a l)
(cond
[(null? l) empty]
[(eqv? a (first l)) (remove-all a (rest l))]
[else (cons (first l) (remove-all a (rest l)))]
))
|#
#lang racket

; Segunda tentativa da lista 1

; Questão 1
(define (concatenar1 l1 l2)
  (if
   (null? l1) l2
   (cons (first l1) (concatenar1 (rest l1) l2))))

; Questão 2
(define (concatenarInv l1 l2)
  (if
   (null? l2) l1
   (cons (first l2) (concatenarInv l1 (rest l2)))))

; Questão 3
(define (concatenar2 l1)
  (if
   (null? l1) empty
   (append (first l1) (concatenar2 (rest l1)))))

; Questão 3-v
(define (concatenar3 . l)
  (concatenar2 l))

; Questão 4
(define (juntar l1 l2)
  (if
   (null? l1) l2
   (cons (first l1) (juntar l2 (rest l1)))))

; Questão 5
(define (adicionarFinal e l)
  (if
   (null? l) (list e)
   (cons (first l) (adicionarFinal e (rest l)))))

; Questão 6
(define (inverter l)
  (letrec
      [(inverter-aux
        (lambda (l1 acc)
          (if
           (null? l1) acc
           (inverter-aux (rest l1) (cons (first l1) acc)))))]
    (inverter-aux l empty)))

; Questão 7
(define (intercala n e1 e2)
  (if
   (>= 0 n) empty
   (cons e1 (intercala (- n 1) e2 e1))))

; Questão 7-v e 8
(define (intercala2 n . e)
  (intercala2-aux n e))

(define (intercala2-aux n e)
  (if
   (or (>= 0 n) (null? e)) empty
   (cons (first e) (intercala2-aux (- n 1) (append (rest e) (list (first e)))))))

; Questão 9
(define (parear e l)
  (if
   (null? l) empty
   (cons (cons e (first l)) (parear e (rest l)))))

; Questão 10
(define (pares l)
  (if
   (null? l) empty
   (append
    (map (lambda (e) (cons (first l) e)) (rest l))
    (pares (rest l)))))

; Questão 12
(define (conjunto? l)
  (not (conjunto?-aux l)))

; Busca duplicatas
(define (conjunto?-aux l)
  (if
   (null? l) #f
   (or
    (ormap (lambda (e) (eqv? (first l) e)) (rest l))
    (conjunto?-aux (rest l)))))

; Questão 13
; A v1 é adequada
(define (prefixo? l1 l2)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(eqv? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
    [else #f]))

; Questão 14
(define (subsequencia? l1 l2)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(eqv? (first l1) (first l2)) (or
                                   (prefixo? (rest l1) (rest l2))
                                   (subsequencia? l1 (rest l2)))]
    [else (subsequencia? l1 (rest l2))]))

; Questão 15
; A v1 está melhor escrita
(define (iguais-lg? lg1 lg2)
  (cond
    [(and (null? lg1) (null? lg2)) #t]
    [(xor (null? lg1) (null? lg2)) #f]
    [(xor (list? (first lg1)) (list? (first lg2))) #f]
    [(nand (list? (first lg1)) (list? (first lg2))) (and
                                                     (eqv? (first lg1) (first lg2))
                                                     (iguais-lg? (rest lg1) (rest lg2)))]
    [else (and
           (iguais-lg? (first lg1) (first lg2))
           (iguais-lg? (rest lg1) (rest lg2)))]))

; Questão 16
(define (remove-todos e l1)
  (letrec
      [(remove-all
        (lambda (a l)
          (cond
            [(null? l) empty]
            [(eqv? a (first l)) (remove-all a (rest l))]
            [else (cons (first l) (remove-all a (rest l)))]
            )))]
    (remove-all e l1)))

; Questão 16 v2
(define (remove-todos2 e l1)
  ((lambda (a l)
    (cond
      [(null? l) empty]
      [(eqv? a (first l)) (remove-todos2 a (rest l))]
      [else (cons (first l) (remove-todos2 a (rest l)))]
      )) e l1))

; Questão extra
#|
Comprima uma lista de elementos usando pares com o número de ocorrências e a
sequência de elementos encontrada
> (comprima '(a a a b b b b c a a a a))
'( (3 . a) (3 . b) (1 . c) (4 . a))

(define (comprima l)
  (if
   (null? l) empty
   (let ([])
|#

; Questão extra
#|
Descomprima, faz o contrário do acima
|#
(define (descomprima l)
  (if
    (null? l) empty
    (append (descomp (caar l) (cdar l)) (descomprima (rest l)))))


(define (descomp n e)
  (if
   (<= n 0) empty
   (cons e (descomp (- n 1) e))))
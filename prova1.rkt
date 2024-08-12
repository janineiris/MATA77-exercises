#lang racket

(define g1-lv '(1 2 3 4 5 6 7 8))

(define g1-la '((1 . 2) (1 . 4) (2 . 3) (4 . 3) 
                (4 . 5) (4 . 6) (3 . 7) (5 . 7) (6 . 8)))

; Primeira questão da prova
(define (gera-sequencia n1 n2 [f identity])
  (if
   (> n1 n2) empty
   (cons (f n1) (gera-sequencia (+ n1 1) n2 f))))

; Segunda questão da prova
(define (adjacentes v la)
  (cond
    [(null? la) empty]
    [(eqv? v (car (first la))) (cons (cdr (first la)) (adjacentes v (rest la)))]
    [(eqv? v (cdr (first la))) (cons (car (first la)) (adjacentes v (rest la)))]
    [else (adjacentes v (rest la))]))

; Terceira questão da prova
(define (graus-ag arv)
  (cond
   [(null? arv) empty]
   [(list? (first arv)) (append (graus-ag (first arv)) (graus-ag (rest arv)))]
   [else (cons (cons (first arv) (length (rest arv))) (graus-ag (rest arv)))]))

; Quarta questão da prova
; Usa membro? e inclui-fim
(define (agl v lv la)
  (cons v (agl-aux v lv la (cons v empty))))

(define (agl-aux v lv la vis)
  (cond
    [(null? vis) empty]
    [(null? la) empty]
    [(eqv? (first vis) (car (first la)))
     (if
      (membro? (cdr (first la)) vis) (agl-aux v lv (rest la) vis)
      (cons (cdr (first la)) (agl-aux v lv (rest la) (inclui-fim (cdr (first la)) vis))))]
    [else (agl-aux v lv la (rest vis))]))

(define (membro? el l)
  (cond
    [(null? l) #f]
    [(eqv? el (first l)) #t]
    [else (membro? el (rest l))]
    ))

(define (inclui-fim e l)
  (if
   (null? l) (cons e empty)
   (cons (first l) (inclui-fim e (rest l)))))

(define (remove* el l)
  (cond
   [(null? l) empty]
   [(eqv? el (first l)) (remove el (rest l))]
   [else (cons (first l) (remove el (rest l)))]))

; Quinta questão da prova
(define (agp v lv la)
  (agp-aux1 v la (cons v empty)))

(define (agp-aux1 v la vis)
  (cond
    [(null? la) vis]
    [(eqv? v (caar la)) (inclui-fim-unique* (agp-aux1 v (rest la) (inclui-fim-unique* (cdar la) vis)) vis)]
    [else (agp-aux1 v (rest la) vis)]))

#|(define (agp-aux2 la vis)
  (cond
    [(null? vis) empty]
    [(
    |#

(define (agp-aux v lv la vis)
  (cond
    [(null? la) vis]
    ;[(eqv? v (car (first la))) (agp-aux (cdr (first la)) lv la (agp-aux v lv la (inclui-fim-unique* (car (first la)) vis)))]
    ;[(membro? v vis) (inclui-fim-unique* (agp-aux
    [(eqv? v (caar la)) (inclui-fim-unique* (cons v (agp-aux )) vis)]
    [else #f]));vis]))

(define (inclui-fim-unique* e l)
  (write e)
  (write l)
  (write "\n")
  (cond
   [(null? l) (cons e empty)]
   [(eqv? e (first l)) l]
   [(null? e) l]
   [(list? e) (inclui-fim-unique* (rest e) (inclui-fim-unique* (first e) l))]
   [else (cons (first l) (inclui-fim-unique* e (rest l)))]))

; Sexta questão da prova
; Usa inclui-fim, dobra-e-aux e mapeia
(define (dobra-e f init . lsts)
  (cond
    [(null? lsts) init]
    ; lsts só tem um elemento
    [(null? (cdr lsts)) (if
                          (null? (car lsts)) init
                          (dobra-e f (f (caar lsts) init) (cdar lsts)))]
    [else (dobra-e-aux f (apply f (inclui-fim init (mapeia car lsts))) (mapeia cdr lsts))]))

(define (dobra-e-aux f init ls)
  (cond
   [(null? ls) init]
   [(null? (car ls)) init]
   [else (dobra-e-aux f (apply f (inclui-fim init (mapeia car ls))) (mapeia cdr ls))]))

(define (mapeia f l)
  (if (null? l)
      '()
      (cons (f (first l))
            (mapeia f (rest l)))))
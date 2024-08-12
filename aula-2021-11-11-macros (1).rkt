#lang racket

; (meu-comando ashsd asdjasdjk asfda)
(define-syntax (meu-comando arg)
  (syntax (+ x 4)))

; > (meu-if-vf ashsd asdjasdjk asfda)
; > (meu-if-vf 3)
; > (define x 4)
; > (eval (meu-if-vf 3))
; 8
(define (meu-if-vf arg)
  (syntax (+ x 4)))

(define-syntax (minha-macro-1 arg)
  (displayln arg)
  (displayln (syntax->datum arg))
  (syntax "Executa esta string")
  )

(define-syntax (minha-macro-2 arg)
  ; (displayln arg)
  (displayln (syntax->datum arg))
  (datum->syntax arg
                 (cadr (syntax->datum arg)))
  )

; Loop
(define-syntax (minha-macro-3 arg)
  ; (displayln arg)
  (displayln (syntax->datum arg))
  arg
  )

; (define stx #'(if x (list true) #f))

(define-syntax (minha-macro-4 arg)
  (datum->syntax arg
                 `(display ,(cadr (syntax->datum arg)))))

(define-syntax (inverta-me stx)
  (datum->syntax stx 
                 (reverse (cdr (syntax->datum stx)))))

(define (meu-if condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

(define-syntax (meu-if-2 arg)
  (define comando (syntax->datum arg))
  (datum->syntax arg
                 `(cond
                    [,(cadr comando) ,(caddr comando)]
                    [else ,(cadddr comando)]))
  )

;;; Aula do Dia 04/11/2021

;(meu-if-3 true (displayln "verdade") (displayln "falso"))

(require (for-syntax racket/list))

(define-syntax (meu-if-3 arg)
  (define comando (syntax->datum arg))
  (datum->syntax arg
                 `(cond
                    [,(second comando) ,(third comando)]
                    [else ,(fourth comando)]))
  )

(begin-for-syntax 

  (define (f1 x) (third x))
  
  )

(define-syntax (meu-if-3-1 arg)
  (define comando (syntax->datum arg))
  (datum->syntax arg
                 `(cond
                    [,(second comando) ,(f1 comando)]
                    [else ,(fourth comando)]))
  )


(define-for-syntax (f2 x) (second x))

(define-syntax (meu-if-3-2 arg)
  (define comando (syntax->datum arg))
  (datum->syntax arg
                 `(cond
                    [,(f2 comando) ,(f1 comando)]
                    [else ,(fourth comando)]))
  )


(require (for-syntax "aula-2021-11-04-lib.rkt"))

(define-syntax (meu-if-3-3 arg)
  (define comando (syntax->datum arg))
  (datum->syntax arg
                 `(cond
                    [,(f2 comando) ,(f1 comando)]
                    [else ,(f3 comando)]))
  )


;; Versão com o match
(require (for-syntax racket/match))

(define-syntax (meu-if-4 arg)
  (match (syntax->datum arg)
    [(list name condition then-expr else-expr)
     (datum->syntax arg
                    `(cond
                       [,condition ,then-expr]
                       [else ,else-expr]))]
    ))


;; Versão com o syntax-case

(define-syntax (meu-if-5 arg)
  (syntax-case arg ()
    [(_ condition then-expr else-expr)
     #'(cond [condition then-expr]
             [else else-expr])]
    ))

;; if then
(define-syntax (meu-if-5-1 arg)
  (syntax-case arg ()
    [(_ condition then-expr else-expr)
     #'(cond [condition then-expr]
             [else else-expr])]
    [(_ condition then-expr)
     #'(cond [condition then-expr])
     ]
    ))

;; (opere 1 2 3 4 5 => *)

(define-syntax (opere arg)
  (syntax-case arg (=>)
    [(_ x ... => op) #'(op x ...)])
  )

;; define-syntax-rule
;; meu if
(define-syntax-rule (meu-if-6 condition then-expr else-expr)
  (cond [condition then-expr]
        [else else-expr])
  )


;; define-syntax-rule
;; opere2
(define-syntax-rule (opere2 x ... => op)
  (op x ...)
  )

;; syntax-rules para fazer o if-then e if-then-else
(define-syntax meu-if-7
  (syntax-rules ()
    [(meu-if-7 condition then-expr else-expr)
     (cond [condition then-expr]
           [else else-expr])]
    [(meu-if-7 condition then-expr)
     (cond [condition then-expr])]
    ))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Aula dia 09/11/2021
;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax (swap-1 stx)
  (syntax-case stx ()
    [(_ x y)
     #'(let ([aux x])
         (set! x y)
         (set! y aux)
         )
     ]
    ))

(define-syntax-rule (swap-3 x y)
  (let ([aux x])
    (set! x y)
    (set! y aux)
    ))


(define-syntax swap-4
  (syntax-rules ()
    [(_ x y)
     (let ([aux x])
       (set! x y)
       (set! y aux)
       )]))


;;; 
(define-syntax (swap-2 stx)
  (syntax-case stx ()
    [(swap2 x y)
     (if (and (identifier? #'x)
              (identifier? #'y))
         #'(let ([tmp x])
             (set! x y)
             (set! y tmp))
         (raise-syntax-error #f
                             "não é identificador"
                             stx
                             (if (identifier? #'x)
                                 #'y
                                 #'x)))]))


(define-syntax (hello-world stx)
  (syntax-case stx ()
    [(_ nome lugar)
     #'(begin
         (define (nome n)
           (printf "Alô\n")
           (for ([i (in-range 0 n)])
             (printf "~a\n" 'nome)))
         (define (lugar n)
           (printf "De\n")
           (for ([i (in-range 0 n)])
             (printf "~a\n" 'lugar))))
     ]))


(define-syntax (hello-world2 stx)
  (syntax-case stx ()
    [(_ nome lugar)
     (with-syntax ([i-nome #'(printf "~a\n" 'nome)]
                   [i-lugar #'(printf "~a\n" 'lugar)])
       #'(begin
           
           (define (nome n)
             (printf "Alô\n")
             (for ([i (in-range 0 n)])
               i-nome))
           
           (define (lugar n)
             (printf "De\n")
             (for ([i (in-range 0 n)])
               i-lugar))
           )
       )
     ]))


(define-syntax (hello-world3 stx)
  (syntax-case stx ()
    [(_ nome lugar)
     (with-syntax ([i-nome #'(printf "~a\n" 'nome)]
                   [i-lugar #'(printf "~a\n" 'lugar)]
                   [nf-nome (datum->syntax #'nome   ;; minha stx
                                           (string->symbol
                                            (format "Hello-~a"
                                                    (syntax->datum #'nome))))]
                   [nf-lugar (datum->syntax #'nome   ;; minha stx
                                            (string->symbol
                                             (format "Hello-~a"
                                                     (syntax->datum #'lugar))))])
       #'(begin
           
           (define (nf-nome n)
             (printf "Alô\n")
             (for ([i (in-range 0 n)])
               i-nome))
           
           (define (nf-lugar n)
             (printf "De\n")
             (for ([i (in-range 0 n)])
               i-lugar))
           )
       )
     ]))

(require (for-syntax racket/syntax))

(define-syntax (hello-world4 stx)
  (syntax-case stx ()
    [(_ nome lugar)
     (with-syntax ([i-nome #'(printf "~a\n" 'nome)]
                   [i-lugar #'(printf "~a\n" 'lugar)]
                   [nf-nome (format-id #'nome
                                       "Hello-~a"
                                       #'nome)]
                   [nf-lugar (format-id #'lugar
                                        "Hello-~a"
                                        #'lugar)])
       #'(begin
           
           (define (nf-nome n)
             (printf "Alô\n")
             (for ([i (in-range 0 n)])
               i-nome))
           
           (define (nf-lugar n)
             (printf "De\n")
             (for ([i (in-range 0 n)])
               i-lugar))
           )
       )
     ]))


(define-syntax (m-cond0 stx)
  (syntax-case stx ()
    [(_) #'(void)]))
  

(define-syntax (m-cond1 stx)
  (syntax-case stx (begin end else)
    [(_) #'(void)]
    [(_ begin cond-expr s-expr end)
     #'(if cond-expr
           s-expr
           (m-cond1))]
    ))

(define-syntax (m-cond2 stx)
  (syntax-case stx (begin end else)
    [(_) #'(void)]
    [(_ begin cond-expr s-expr end)
     #'(if cond-expr
           s-expr
           (m-cond2))]
    [(_ begin cond-expr s-expr end else e-expr)
     #'(if cond-expr
           s-expr
           e-expr)]
    ))


(define-syntax (m-cond3 stx)
  (syntax-case stx (begin end else)
    [(_) #'(void)]
    [(_ begin cond-expr s-expr end)
     #'(if cond-expr
           s-expr
           (m-cond3))]
    [(_ begin cond-expr s-expr end else e-expr)
     #'(if cond-expr
           s-expr
           e-expr)]
    [(_ begin cond-expr s-expr end resto ...)
     #'(if cond-expr
           s-expr
           (m-cond3 resto ...))]
    ))




  


(define-syntax (meu-cond-v4 stx)
  (syntax-case stx ()
    [(_) #'(void)]                                  ; T1
    [(_ {{cond-expr s-expr}})                       ; T2
     #'(if cond-expr
           s-expr
           (meu-cond-v4))]
    [(_ {{cond-expr s-expr}} {{else-stmt final-expr}}) ; T3
     (eq? 'else (syntax->datum #'else-stmt))
     #'(if cond-expr
           s-expr
           final-expr)]
    [(_ {{cond-expr s-expr}} outras-cond ...)       ; T4
     #'(if cond-expr
           s-expr
           (meu-cond-v4 outras-cond ...))]
    ))

(define (compara2 x y)
  (meu-cond-v4 {{(> x y) 'maior}}
               {{(< x y) 'menor}}
               {{else 'igual}}
               ))

(define-syntax (meu-cond-v5 stx)
  (syntax-case stx (else)
    [(_) #'(void)]                                  ; T1
    [(_ {{cond-expr s-expr}})                       ; T2
     #'(if cond-expr
           s-expr
           (meu-cond-v5))]
    [(_ {{cond-expr s-expr}} {{else final-expr}})   ; T3
     #'(if cond-expr
           s-expr
           final-expr)]
    [(_ {{cond-expr s-expr}} outras-cond ...)       ; T4
     #'(if cond-expr
           s-expr
           (meu-cond-v5 outras-cond ...))]
    ))

(define (compara3 x y)
  (meu-cond-v5 {{(> x y) 'maior}}
               {{(< x y) 'menor}}
               {{else 'igual}}
               ))


(define-syntax (meu-cond-v6 stx)
  (syntax-case stx (else begin end)
    [(_) #'(void)]                                    ; T1
    [(_ begin cond-expr s-expr end)                   ; T2
     #'(if cond-expr
           s-expr
           (meu-cond-v6))]
    [(_  begin cond-expr s-expr end else final-expr ) ; T3
     #'(if cond-expr
           s-expr
           final-expr)]
    [(_ begin cond-expr s-expr end outras-cond ...)   ; T4
     #'(if cond-expr
           s-expr
           (meu-cond-v6 outras-cond ...))]
    ))

(define (compara4 x y)
  (meu-cond-v6 begin
               (> x y) 'maior
               end
               begin
               (< x y) 'menor
               end
               else
               'igual
               ))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Aula dia 11/11/2021
;;;;;;;;;;;;;;;;;;;;;;;



(define-syntax (define-hifenizado stx)
  (syntax-case stx ()
    [(_ a b (args ...) corpo0 resto ...)
     (syntax-case 
         (datum->syntax #'a   ;; nova stx
                        (string->symbol
                         (format "~a-~a"
                                 (syntax->datum #'a)
                                 (syntax->datum #'b))))
       ()                  ;; literais
       [nome  ;; template
        #'(define (nome args ...)
            corpo0 resto ...)])]))



(define-syntax (d-h* stx)
  (syntax-case stx ()
    [(_ (nome) (args ...) corpo resto ...)
     #'(define (nome args ...) corpo resto ...)
     ]
    [(_ (nome1 nome2 nomes ...) (args ...) corpo resto ...)
     (with-syntax ([nome1-2
                    (format-id #'nome1
                               "~a-~a"
                               #'nome1 #'nome2)])
       #'(d-h* (nome1-2 nomes ...) (args ...) corpo resto ...))
     ]))

(define-syntax (define-hifenizado* stx)
  (syntax-case stx ()
    [(_ (nome) (args ...) corpo0 resto ...)
     #'(define (nome args ...) corpo0 resto ...)]
    [(_ (nome1 nome2 outros-nomes ...) (args ...) corpo0 resto ...)
     (with-syntax
         ([nome12 (format-id #'nome1 "~a-~a" #'nome1 #'nome2)])
       #'(define-hifenizado*
           (nome12 outros-nomes ...)
           (args ...)
           corpo0
           resto ...))
     ]
    ))

(define-hifenizado* (foo bar teste longo) (x) (+ x x))



(define-syntax (cc stx)
  (syntax-case stx ()
    [(_ nome-h)
     (with-syntax ([nome-cc
                    (format-id #'nome-h
                               (camel-case
                                (symbol->string 
                                 (syntax->datum #'nome-h))))])
       #'(define nome-cc nome-h))]))



(require (for-syntax racket/string))

;; esta sequencia contém as funções que recebem uma string de
;; caracteres separados por hífen e faz camel case dela

;; (camel-case string char)
;; recebe uma string de caracteres separados por hífen (default) ou
;; outro caracter, e transforma a mesma em uma string camel case.
;;
;; Exemplos:
;; (camelCase "manoel-gomes-mendonca")
;; > "manoelGomesMendonca"
;; (camelCase "manoel$gomes$mendonca" "$")
;; > "manoelGomesMendonca"
;;
(begin-for-syntax
  
  (define (camel-case str [c "-"])
    (let ([str-list (string-split str c)]) ; 
      (cond
        [(null? str-list) str]
        [(null? (cdr str-list)) str]
        [else
         (string-append (car str-list)
                        (foldr string-append
                               ""
                               (map coloque-maiuscula
                                    (cdr str-list))))])))

  ;; (coloque-maiuscula string)
  ;; (transforma o primeiro caractere de uma string em maiúscula)
  (define (coloque-maiuscula str)
    (string-append
     (string-upcase (substring str 0 1))
     (substring str 1)))
  
  )
#|
(define-syntax (camel-case-m stx)
  (syntax-case stx ()
      [(_ func)
       (with-syntax
           ([nome
                (datum->syntax #'func
                      (string->symbol
                         (camel-case
                             (symbol->string
                              (syntax->datum #'func)))))])
         #'(define nome func)
       )]
    ))
|#

(define-syntax (camel-case-1 stx)
  (syntax-case stx ()
    [(_ func)
     (with-syntax
         ([nome (format-id #'func
                           (camel-case
                            (symbol->string
                             (syntax->datum #'func))))
                ])
       #'(define nome func)
       )]
    ))

;; teste
(define (minha-funcao a b c)
  (+ a b c))

(camel-case-1 minha-funcao)

;;; Fim de camel case 1




(define-syntax (cc* stx)
  (syntax-case stx ()
    [(_ (nome) (args ...) corpo resto ...)
     #'(define (nome args ...) corpo resto ...)
     ]
    [(_ (nome1 nome2 nomes ...) (args ...) corpo resto ...)
     (with-syntax* ([nomec2
                     (datum->syntax #'nome2 
                                    (string->symbol
                                     (coloque-maiuscula 
                                      (symbol->string
                                       (syntax->datum #'nome2)))))
                     ]
                    [nome1c2
                     (format-id #'nome1
                                "~a~a"
                                #'nome1
                                #'nomec2)])
       #'(cc* (nome1c2 nomes ...) (args ...) corpo resto ...))
     ]))













;;; Define camel case

(define-syntax (define-camel-case stx)
  (syntax-case stx ()
    [(_ (nome) (args ...) corpo0 resto ...)
     #'(define (nome args ...) corpo0 resto ...)]
    [(_ (nome1 nome2 outros-nomes ...) (args ...) corpo0 resto ...)
     (with-syntax
         ([nome12 (datum->syntax #'nome1
                                 (string->symbol
                                  (string-append
                                   (symbol->string
                                    (syntax->datum #'nome1))
                                   (coloque-maiuscula
                                    (symbol->string
                                     (syntax->datum #'nome2))))))])
       #'(define-camel-case
           (nome12 outros-nomes ...)
           (args ...)
           corpo0
           resto ...))
     ]
    ))

(define-camel-case (minha nova funcao) (a b c)
  (+ a b c))



(define-syntax (define-camel-case2 stx)
  (syntax-case stx ()
    [(_ nome (args ...) corpo0)
     #'(define (nome args ...) corpo0)]
    [(_ nome1 nome2 outros-nomes ... (args ...) corpo0)
     (with-syntax
         ([nome12 (datum->syntax #'nome1
                                 (string->symbol
                                  (string-append
                                   (symbol->string
                                    (syntax->datum #'nome1))
                                   (coloque-maiuscula
                                    (symbol->string
                                     (syntax->datum #'nome2))))))])
       #'(define-camel-case2
           nome12 outros-nomes ...
           (args ...)
           corpo0
           ))
     ]
    ))

(define-camel-case2 minha outra funcao (a b c)
  (+ a b c))

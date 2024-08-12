#lang racket

(require (for-syntax racket/list))
(require (for-syntax racket/string))

; Prova 2

; Questão 1
(define-syntax-rule (or-case-v0 val-expr ())
   #f)

; Questão 2
;(or-case val-expr (val-comp ...))

(define-syntax (or-case stx)
  (syntax-case stx ()
    [(_ val-expr ()) #'#f]
    [(_ val-expr (first-val val-comp ...))
     #'(if (equal? val-expr first-val)
           #t
           (or-case val-expr (val-comp ...)))]))
     ;(let ([lista-valores (syntax->datum #'(val-comp ...))]
           ;[v (syntax->datum #'val-expr)])
       ;#'(eval-syntax #'val-expr))]))
       ;(lista-inclui (eval-syntax #'val-expr) lista-valores))]))

#|(define-for-syntax (lista-inclui v l)
  (cond
    [(not (null? l))
  (displayln v)
  (displayln (first l))
  (displayln (equal? v (first l)))
  (display (number? v))
  (display (string? v))
  (display (list? v))
  (display (number? (first l)))
  (display (string? (first l)))
  (displayln (list? (first l)))])
  (cond
    [(null? l) #'#f]
    [(equal? (eval-syntax (syntax #'v)) (eval-syntax (syntax #'(first l)))) #'#t]
    [else (lista-inclui v (rest l))]))
|#


; Questão 3
(define-syntax-rule (meu-case-v0 val-expr)
  (void))

; Questão 4
; Usando import de racket list
(define-syntax (meu-case-v1 stx)
  (syntax-case stx ()
    [(_ val-expr) #'(void)]
    [(_ val-expr [(val-comp ...) then-expr then-exprs ...])
     #'(cond
         [(or-case val-expr (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))])]))
       ;#'(if is-match
        ;   (or last-then then-expr)
         ;  (void)))]))
       ;#'(cond
       ;    [is-match (or last-then then-expr)]))]))
               ;(if (null? then-list)
                ;          #f
                 ;         (last #'then-list))])
            ;#'(cond
            ;    [(or-case val-expr (val-comp ...))
             ;    last-then]))]))
                 ;(or last-then then-expr)]))]))
     ;(if #'(syntax-eval (or-case #'val-exp (#'(val-comp ...))))
     ;      #'then-expr
     ;      (void))]))
     ;(if
       ;#'(or-case #'val-exp (#'(val-comp ...))) #'then-expr #'(void))]))

; Questão 5
(define-syntax (meu-case-v2 stx)
  (syntax-case stx ()
    [(_ val-expr) #'(void)]
    [(_ val-expr [(val-comp ...) then-expr then-exprs ...])
     #'(meu-case-v1 val-expr [(val-comp ...) then-expr then-exprs ...])]
    [(_ val-expr [(val-comp ...) then-expr then-exprs ...] second-clause other-clauses ...)
      #'(cond
         [(or-case val-expr (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))]
         [else (meu-case-v2 val-expr second-clause other-clauses ...)])]))

; Questão 6
(define-syntax (meu-case stx)
  (syntax-case stx (else)
    [(_ val-expr) #'(void)]
    [(_ val-expr [(val-comp ...) then-expr then-exprs ...])
      #'(meu-case-v1 val-expr [(val-comp ...) then-expr then-exprs ...])]
    [(_ val-expr [(val-comp ...) then-expr then-exprs ...] [else else-expr else-exprs ...])
      #'(cond
         [(or-case val-expr (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))]
         [else (let* ([then-list (syntax->datum #'(else-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then else-expr))])]
    [(_ val-expr [(val-comp ...) then-expr then-exprs ...] second-clause other-clauses ...)
      #'(cond
         [(or-case val-expr (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))]
         [else (meu-case val-expr second-clause other-clauses ...)])]))

#|
(define-syntax (meu-if-3 arg)
  (define comando (syntax->datum arg))
  (datum->syntax arg
                 `(cond
                    [,(second comando) ,(third comando)]
                    [else ,(fourth comando)]))
  )|#

; Questão 7
(define-syntax (or-case-lit stx)
  (syntax-case stx ()
    [(_ id val-lit ()) #'#f]
    [(_ id val-lit (first-val val-comp ...))
     #'(if (equal? 'val-lit 'first-val)
           #t
           (or-case-lit id val-lit (val-comp ...)))]))

; Questão 8
(define-syntax (enum-case2 stx)
  (syntax-case stx (else)
    [(_ id val-lit) #'(void)]
    [(_ id val-lit [(val-comp ...) then-expr then-exprs ...])
      #'(cond
         [(or-case-lit id val-lit (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))])]
    [(_ id val-lit [(val-comp ...) then-expr then-exprs ...] [else else-expr else-exprs ...])
      #'(cond
         [(or-case-lit id val-lit (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))]
         [else (let* ([then-list (syntax->datum #'(else-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then else-expr))])]
    [(_ id val-lit [(val-comp ...) then-expr then-exprs ...] second-clause other-clauses ...)
      #'(cond
         [(or-case-lit id val-lit (val-comp ...))
          (let* ([then-list (syntax->datum #'(then-exprs ...))]
                 [last-then (and (not (null? then-list)) (last then-list))])
            (or last-then then-expr))]
         [else (enum-case2 id val-lit second-clause other-clauses ...)])]))

; Questão 9
(define-syntax (testa-completude stx)
  (syntax-case stx (else)
    [(_ enum-id val-lit (lista-args ...))
     (let* ([args (to-symbol-list (syntax->list #'(lista-args ...)))]
            [list-enum (syntax-local-value #'enum-id
                                          (lambda ()
                                            (error 'enum-case "enumeração <~a> não existe" (syntax-to-string #'enum-id))))]
            [args-ausentes (checa-listas args list-enum empty)]
            [enums-n-tratados (checa-listas list-enum args empty)]
            [str-args (if (null? args-ausentes)
                          ""
                          (list-to-str args-ausentes))]
            [str-enums (if (null? enums-n-tratados)
                          ""
                          (list-to-str enums-n-tratados))])
       (cond
         [(not (null? enums-n-tratados))
          (error 'enum-case "enumerações <~a> de <~a> não foram tratadas" str-enums (syntax-to-string #'enum-id))]
         [(not (null? args-ausentes))
          (error 'enum-case "argumentos <~a> não existem em <~a>" str-args (syntax-to-string #'enum-id))]
         [else #'(void)]))]
    [(_ enum-id val-lit [(val-comp ...) then-expr then-exprs ...] [else else-expr else-exprs ...] (lista-args ...))
     (let* ([args (to-symbol-list (syntax->list #'(lista-args ...)))]
            [values (to-symbol-list (syntax->list #'(val-comp ...)))]
            [arg-values (append args values)]
            [list-enum (syntax-local-value #'enum-id
                                           (lambda ()
                                             (error 'enum-case "enumeração <~a> não existe" (syntax-to-string #'enum-id))))]
            [args-ausentes (checa-listas arg-values list-enum empty)]
            [str-args (if (null? args-ausentes)
                          ""
                          (list-to-str args-ausentes))])
       (if (not (null? args-ausentes))
             (error 'enum-case "argumentos <~a> não existem em <~a>" str-args (syntax-to-string #'enum-id))
             #'(void)))]
    [(_ enum-id val-lit [(val-comp ...) then-expr then-exprs ...] other-clauses ... (lista-args ...))
     (let* ([args (to-symbol-list (syntax->list #'(lista-args ...)))]
            [values (to-symbol-list (syntax->list #'(val-comp ...)))]
            [arg-values (append args values)])
       #`(testa-completude enum-id val-lit other-clauses ... #,(append args values)))]))

(define-for-syntax (syntax-to-string s)
  (symbol->string (syntax-e s)))

(define-for-syntax (to-symbol-list l)
  (if (null? l)
      empty
      (map (lambda (x) (syntax-e x) )l)))

; Requer racket/string
(define-for-syntax (list-to-str l)
  (if (null? l)
      ""
      (string-join
       (map (lambda (x) (symbol->string x)) l)
       ",")))
            
(define-for-syntax (membro? el l)
  (cond
    [(null? l) #f]
    [(equal? el (first l)) #t]
    [else (membro? el (rest l))]))

; usar (checa-listas list-args list-enum list-enum empty)
; e (checa-listas list-enum list-args list-args empty)
; pra gerar as listas das duas primeiras mensagens de erro
(define-for-syntax (checa-listas list-args list-enum args-ausentes)
  #|(displayln list-args)
  (displayln "args")
  (displayln list-enum)
  (displayln "num")|#
  (cond
    [(null? list-args) args-ausentes]
    [(membro? (first list-args) list-enum)
     (checa-listas (rest list-args) list-enum args-ausentes)]
    [else (checa-listas (rest list-args) list-enum (cons (first list-args) args-ausentes))]))

; Códigos para teste da Questão 9
(define-syntax-rule (define-enum enum-id [case-id ...])
    (define-syntax enum-id (quote [case-id ...]))
  )

(define-enum animais [gato cachorro elefante pato arara])
(define-enum frutas [banana manga macã pera jaca limão])

(define-syntax (enum-case stx)
  (begin
    (syntax-case stx ()
      [(_ corpo ...)
       #'(begin
           ; verifica completude da sintaxe da enumeração
           (testa-completude corpo ... ())
           ; processa a forma sintática
           (enum-case2 corpo ...))]
      )
    ))
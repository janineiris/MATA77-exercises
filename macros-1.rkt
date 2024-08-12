#lang racket

(require (for-syntax racket/string racket/syntax))

(define-syntax (define-hifenizado* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let ([name-stxs (syntax->list #'(names ...))])
       (with-syntax
           ([name (datum->syntax
                   (car name-stxs)
                   (string->symbol
                    (string-join
                     (for/list ([name-stx name-stxs])
                       (symbol->string (syntax-e name-stx)))
                     "-")))])
         #'(define (name args ...)
             body0 body ...)))]))

; Exercício Camel Case 1
(define-syntax (camel-case-m stx)
  (syntax-case stx ()
    [(_ nomeHifenizado)
     (with-syntax
         ([nomeCamelCase (datum->syntax
                          stx
                          (string->symbol
                           (regexp-replace
                            #rx"^."
                            (string-join
                             (for/list ([nome (string-split (symbol->string (syntax-e #'nomeHifenizado)) "-")])
                               (regexp-replace #rx"^." nome string-upcase))
                             "")
                            string-downcase)))])
       #'(define nomeCamelCase nomeHifenizado))]))


#|
(define-syntax (pipe stx)
  (syntax-case stx ()
    [(_ v0 expr ...)]
|#

;;;;;;;;;; Prova 2021.1

(define-syntax-rule (pipe-v1 v0)
  v0)

; 2ª versão da pipe-v1
(define-syntax (pipe-v1-2 stx)
  (syntax-case stx ()
    [(_ v0) #'v0]))

; pipe-v2
(define-syntax (pipe-v2 stx)
  (syntax-case stx ()
    [(_ v0) #'v0]
    [(_ v0 (fn arg1 args ...)) #'(fn v0 arg1 args ...)]))

; pipe-v3
(define-syntax (pipe-v3 stx)
  (syntax-case stx ()
    [(_ v0) #'v0]
    [(_ v0 (fn arg1 args ...)) #'(fn v0 arg1 args ...)]
    [(_ v0 (fn arg1 args ...) fns ...)
     #'(pipe-v3 (fn v0 arg1 args ...) fns ...)]))

; pipe-v4
(define-syntax (pipe-v4 stx)
  (syntax-case stx ()
    [(_ v0) #'v0]
    [(_ v0 (fn arg1 args ...)) #'(fn v0 arg1 args ...)]
    [(_ v0 (fn arg1 args ...) fns ...)
     #'(pipe-v4 (fn v0 arg1 args ...) fns ...)]
    [(_ v0 id-fn args ...) #'(pipe-v4 (id-fn v0) args ...)]))
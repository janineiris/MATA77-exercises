#lang racket

(require (for-syntax racket/syntax))
(require (for-syntax racket/list))


(define-syntax (hello-world stx)
  (define xs (syntax->datum stx))
  (define nome (second xs))
  (define lugar (third xs))
  (displayln nome)
  (displayln (syntax? nome))
  #'(define ('nome n)
                  (displayln "Hello")
                  (for ([i (in-range n)])
                    (printf "~a\n" 'nome)))
  #'(define ('lugar n)
                  (displayln "From")
                  (for ([i (in-range n)])
                    (printf "~a\n" 'lugar))))
  #|
  (datum->syntax stx `(define (,nome n)
                  (displayln "Hello")
                  (for ([i (in-range n)])
                    (displayln ,nome))))
  (datum->syntax stx `(define (,lugar n)
                  (displayln "From")
                  (for ([i (in-range n)])
                    (displayln ,lugar)))))
|#

#|
(define-syntax (hello-world stx)
  (syntax-case stx ()
    [(_ nome lugar)
     (with-syntax
         ([
|#
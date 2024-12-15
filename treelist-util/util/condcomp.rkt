#lang racket/base

(provide if-not-exported)

(require (for-syntax racket/base
                     racket/syntax)
         syntax/parse/define)

(define-syntax-parser if-not-exported
  [(if-not-exported m:expr x:id body:expr ...)
   (define s (syntax-e #'x))
   (cond
     [(ormap (λ (l) (member s (cdr l)))
             (syntax-local-module-exports (syntax->datum #'m)))
      #'(void #'x)]
     [else #'(begin body ...)])])

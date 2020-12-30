#lang racket

(require
  syntax/parse/define
  ee-lib/define
  (for-syntax
    racket/syntax
    syntax/parse
    ee-lib))

(provide (all-defined-out))

(begin-for-syntax
  (struct var (name))

  (define (compile-var v)
    (check-ref v)
    (define v^ (lookup v))
    (var-name v^))

  (define (check-ref v)
    (when (not (lookup v var?))
      (raise-syntax-error #f "unbound reference" v)))

  (define (check-expr stx)
    (syntax-parse stx
      [v:id     (check-ref #'v)]
      [_:number (void)]))

  (define (compile-expr stx)
    (syntax-parse stx
      [v:id     (compile-var #'v)]
      [_:number this-syntax])))

(define-syntax-rule (begin-mini-dsl body ...)
  (begin body ...))

(define-syntax-rule (use path)
  (require path))

(define-syntax show
  (syntax-parser
    [(_ src:id)
     (if (not (eq? (syntax-local-context) 'expression))
       #'(#%expression (show src)) ; delay to second pass of module expansion
       (with-syntax ([v (compile-var #'src)])
         #'(println v)))]))

(define-syntax assign
  (syntax-parser
    [(_ dest:id src)
     (define/syntax-parse compiled-dest (generate-temporary #'dest))
     #'(begin
         (define-syntax dest (var #'compiled-dest))
         (provide dest)
         (define compiled-dest (check-and-compile-expr src)))]))

(define-syntax check-and-compile-expr
  (syntax-parser
    [(_ e)
     (check-expr #'e)
     (compile-expr #'e)]))

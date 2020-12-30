#lang racket

(require
  syntax/parse/define
  (for-syntax
    racket/syntax
    syntax/parse
    ee-lib))

(provide (all-defined-out))

(define-syntax-parser assign
  [(_ dest src)
   #:with b-name (syntax-property this-syntax 'binding-name)
   #'(begin
       (provide b-name)
       (define dest src)
       (define-syntax b-name (var #'dest)))])

(define-syntax-rule (show src)
  (println src))

(define-syntax (use stx)
  (raise-syntax-error #f "should be used in a begin-mini-dsl form" stx))

(define-syntax-parser begin-mini-dsl
  [(_ body ...)
   #`(begin
       (compile-use body) ...
       (compile-mini-dsl body ...))])

(define-syntax-parser compile-use
  #:literals [use]
  [(_ (use path)) #'(require path)]
  [_              #'(begin)])

(define-syntax (compile-mini-dsl stx)
  (check stx))

(begin-for-syntax
  (struct var (name))

  (define (bind-var! v)
    (define v^ (generate-temporary v))
    (bind! v (var v^))
    v^)

  (define (lookup-var v)
    (define v^ (lookup v var?))
    (unless v^
      (raise-syntax-error #f "variable not found" v))
    (var-name v^))

  (define (decorate name stx)
    (syntax-property stx 'binding-name name))

  (define (check stx)
    (syntax-parse stx
      #:literals [compile-mini-dsl assign show use]

      [(compile-mini-dsl body ...)
       (with-scope sc
         #`(begin
             #,@(for/list ([it (attribute body)])
                  (check (add-scope it sc)))))]

      [(assign dest src:id)
       #:with dest^ (bind-var! #'dest)
       #:with src^  (lookup-var #'src)
       (decorate #'dest #'(assign dest^ src^))]

      [(assign dest src)
       #:with dest^ (bind-var! #'dest)
       (decorate #'dest #'(assign dest^ src))]

      [(show src:id)
       #:with src^ (lookup-var #'src)
       #'(show src^)]

      [(use _)
       #'(begin)]

      [_
       stx])))

#lang racket

(require
  syntax/parse/define
  (for-syntax
    syntax/parse
    ee-lib))

(provide (all-defined-out))

(define-syntax-rule (assign dest src)
  (begin
    (provide dest)
    (define dest src)))

(define-syntax-rule (show src)
  (println src))

(define-syntax (use stx)
  (raise-syntax-error #f "should be used in a begin-mini-dsl form" stx))

(define-syntax-parser begin-mini-dsl
  [(_ body ...)
   (check (for/fold ([acc (syntax-local-introduce this-syntax)])
                    ([it  (in-list (attribute body))])
            (syntax-parse it
              #:literals [use]
              [(use path) (syntax-local-lift-require #'path acc)]
              [_          acc])))])

(begin-for-syntax
  (struct var (name))

  (define (bind-var! v)
    (define v^ (gensym))
    (bind! v (var v^))
    ; (syntax-local-lift-provide v)
    v^)

  (define (lookup-var v)
    (define v^ (lookup v var?))
    (unless v^
      (raise-syntax-error #f "variable not found" #'v))
    (var-name v^))

  (define (check stx)
    (syntax-parse stx
      #:literals [begin-mini-dsl assign show use]

      [(begin-mini-dsl body ...)
       (with-scope sc
         #`(begin
             #,@(for/list ([it (attribute body)])
                  (check (add-scope it sc)))))]

      [(assign dest src:id)
       #:with dest^ (bind-var! #'dest)
       #:with src^  (lookup-var #'src)
       #`(assign dest^ src^)]

      [(assign dest src)
       #:with dest^ (bind-var! #'dest)
       #`(assign dest^ src)]

      [(show src:id)
       #:with src^  (lookup-var #'src)
       #`(show src^)]

      [(use _)
       #'(begin)]

      [_
       stx])))

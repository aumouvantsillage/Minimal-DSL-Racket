#lang racket

(require
  syntax/parse/define
  ee-lib/define
  (for-syntax
    racket/syntax
    syntax/parse
    ee-lib))

(provide (all-defined-out))

(define-literal-forms
  mini-dsl-lits
  "should be used in a begin-mini-dsl form"
  [use show assign])

(define-syntax-parser begin-mini-dsl
  [(_ body ...)
   #'(begin
       (check-pass1 body) ...
       (check-pass2 body) ...
       (compile body) ...)])

(begin-for-syntax
  (struct var (name))

  (define (compile-var v)
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

(define-syntax check-pass1
  (syntax-parser
    #:literal-sets (mini-dsl-lits)
    [(assign dest:id _)
     (define/syntax-parse compiled-id (generate-temporary #'dest))
     #`(begin
         (define-syntax dest (var #'compiled-id))
         (provide dest))]
    [(use path)
     #'(require path)]
    [_
     #'(begin)]))

(define-syntax check-pass2
  (syntax-parser
    #:literal-sets (mini-dsl-lits)
    [(assign dest:id src:id) (check-expr #'src)]
    [(show src:id) (check-ref #'src)]
    [_ (void)])
  #'(begin))

(define-syntax compile
  (syntax-parse stx
    #:literal-sets (mini-dsl-lits)
    [(assign dest:id src:id)
     (define/syntax-parse dest-c (compile-var #'dest))
     (define/syntax-parse src-c (compile-expr #'src))
     #'(define dest-c src-c)]
    [(show src:id)
     #:with src-c (compile-var #'src)
     #'(println src-c)]
    [_ #'(begin)]))

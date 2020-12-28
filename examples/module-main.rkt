#lang racket

(require "../main.rkt")

(begin-mini-dsl
  (use "module-lib.rkt")
  (assign b a)
  (show b))

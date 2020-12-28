#lang racket

(require "../main.rkt")

(begin-mini-dsl
  (assign a 45)
  (assign b a)
  (show b))

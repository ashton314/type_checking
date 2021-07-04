#lang racket/base

(require "kanren.rkt")

(define (father p s)
  (conde ((== p 'paul) (== s 'jason))
         ((== p 'john) (== s 'henry))
         ((== p 'jason) (== s 'tom))
         ((== p 'peter) (== s 'brian))
         ((== p 'tom) (== s 'peter))))

(define (grandfather g s)
  (fresh (p) (conj+ (father g p) (father p s))))

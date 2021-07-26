#lang racket/base

(require "kanren.rkt")
(require (except-in racket/match ==) racket/pretty (only-in racket/list append-map))

;;; Phase 1: constraint generation
(struct type-constraint (lhs rhs) #:transparent)
(struct tc/= type-constraint () #:transparent)
(struct tc/< type-constraint () #:transparent)

(struct type-type () #:transparent)
(struct tt/expr type-type (expr) #:transparent)
(struct tt/var type-type (name) #:transparent)
(struct tt/num type-type () #:transparent)
(struct tt/bool type-type () #:transparent)
(struct tt/arrow type-type (domain range) #:transparent)

(define (gen-constraints expr)
  (match expr
    [(? number?) (list (tc/= (tt/expr expr) (tt/num)))]
    [(? boolean?) (list (tc/= (tt/expr expr) (tt/bool)))]
    [(? symbol?) (list (tc/= (tt/expr expr) (tt/var expr)))]
    [`(,(or '+ '- '* '/) ,lhs ,rhs) (append (gen-constraints lhs) (gen-constraints rhs)
                                            (list (tc/= (tt/expr lhs) (tt/num))
                                                  (tc/= (tt/expr rhs) (tt/num))
                                                  (tc/= (tt/expr expr) (tt/num))))]
    [`(zero? ,e) (append (gen-constraints e)
                         (list (tc/= (tt/expr expr) (tt/bool))))]
    [`(,(or 'lambda 'λ) (,xs ...) ,body)
     (append (gen-constraints body)
             (tc/= expr (tt/arrow (map (λ (x) (tt/var x)) xs) (tt/expr body))))]
    [`(,ef ,xs ...)
     (append (gen-constraints ef)
             (append-map gen-constraints xs)
             (list (tc/= (tt/expr ef) (tt/arrow (map tt/expr xs) (tt/expr expr)))))]))

(define my-progn
  '(+ 1 n))

(gen-constraints my-progn)

(define (subtype? v t)
  (if (and (type-type? v) (type-type? t))
      ))

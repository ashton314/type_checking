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
             (list (tc/= (tt/expr expr) (tt/arrow (map (λ (x) (tt/var x)) xs) (tt/expr body)))))]
    [`(,ef ,xs ...)
     (append (gen-constraints ef)
             (append-map gen-constraints xs)
             (list (tc/= (tt/expr ef) (tt/arrow (map tt/expr xs) (tt/expr expr)))))]))

(define (i v [label 'v])
  (printf "~a: " label)
  (pretty-print v)
  v)

(define my-progn
  '((λ (a) (+ a 1)) 5))

(gen-constraints my-progn)

;; (define (tt->kanren constraints subs)
;;   (if (null? constraints)
;;       (values constraints subs)
;;       (match (car constraints)
;;         [(tc/= (tt/expr e) type)
;;          ])))

;;; Make it so that numbers unify with their type
(define (my-eqv? a b)
  (or (and (tt/num? a) (number? b))
      (and (tt/num? b) (number? a))
      (and (tt/bool? b) (boolean? a))
      (and (tt/bool? b) (boolean? a))
      (eqv? a b)))

(equivalent? my-eqv?)

;; TODO: type check a simple program

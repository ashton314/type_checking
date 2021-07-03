#lang racket/base

(provide (all-defined-out))

(define empty-state '(() . 0))

(define (var c) (vector c))             ; turn c into a variable
(define (var? x) (vector? x))           ; vector predicate
(define (var=? x₁ x₂)
  ;; variables are equal if they have the same value up in front
  (= (vector-ref x₁ 0) (vector-ref x₂ 0)))

(define (walk u subst)
  ;; u is a variable and subst is an association list
  ;; This function tries to find a binding for u in the substitution assoc list.
  (let ([pr (and (var? u) (assp (λ (v) (var=? u v)) subst))])
    ;; If pr comes back with a value, that means that u is a reference
    ;; to some other variable, (transitively, through the recursive
    ;; call and the var? predicate) so we recur to get the final
    ;; binding for u.
    (if pr (walk (cdr pr) subst) u)))

;; Needed because Racket doesn't implement R6RS's assp function
;; natively afaik
(define (assp ? lst)
  (if (null? lst)
      #f
      (if (? (caar lst)) (car lst) (assp ? (cdr lst)))))

;; Extend the substitution association list
(define (ext-s x v s) (cons `(,x . ,v) s))

;; Goal constructor 1: unify
(define (== u v)
  (λ (s/c)                              ; s/c is a state, I believe
    (let ([s (unify u v (car s/c))])
      (if s (unit (cons s (cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

;; unify :: term -> term -> substitution (an assoc list)
(define (unify u v s)
  ;; See if u and v unify under substitutions s
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]          ; if u is a variable, make u point to v
      [(var? v) (ext-s v u s)]          ; ditto for v: we know the var is unbound thanks to walk
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (equiv? u v) s)])))

;; What shape does a state have? It is
;;     (cons substitutions: assoc-list, var-counter: integer)

;; Goal constructor 2: call/fresh
;; call/fresh :: (a -> goal) -> goal
(define (call/fresh f)
  ;; Take a function 
  (λ (s/c)                              ; a state
    (let ([c (cdr s/c)])
      ((f (var c)) (cons (car s/c) (+ c 1))))))

;; Disjoint and Conjoint goal constructors
;; disj | conj :: goal -> goal -> goal
(define (disj g₁ g₂)
  (λ (s/c) (mplus (g₁ s/c) (g₂ s/c))))

(define (conj g₁ g₂)
  (λ (s/c) (bind (g₁ s/c) g₂)))

;; mplus: merge streams
(define (mplus $₁ $₂)
  (cond
    [(null? $₁) $₂]
    [(procedure? $₁) (λ () (mplus $₂ ($₁)))]
    [else (cons (car $₁) (mplus (cdr $₁) $₂))]))

;; bind: given a stream and a goal, walk the stream with the goal
(define (bind $ g)
  (cond
    [(null? $) mzero]
    [(procedure? $) (λ () (bind ($) g))]
    [else (mplus (g (car $)) (bind (cdr $) g))]))

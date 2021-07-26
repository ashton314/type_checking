#lang racket/base

(require racket/pretty)

(provide (all-defined-out))

(define empty-state `(() . var0))

(struct var (name) #:transparent)

;; (define (var c) c)             ; turn c into a variable
;; (define (var? x) (symbol? x))  ; vector predicate
(define (var-maybe v) (if (var? v) v (var v)))
(define (var=? x₁ x₂) (eq? (var-name x₁) (var-name x₂)))
(define (fresh-var) (var (gensym 'v)))

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
  ;; (pretty-print `((u . ,u) (v . ,v)))
  ;; (pretty-print `(s . ,s))
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
      [else (and (eqv? u v) s)])))

;; What shape does a state have? It is
;;     (cons substitutions: assoc-list, var-counter: integer)

;; Goal constructor 2: call/fresh
;; call/fresh :: (a -> goal) -> goal
(define (call/fresh f)
  ;; Take a function 
  (λ (s/c)                              ; a state
    (let ([c (cdr s/c)])
      ((f (var-maybe c)) (cons (car s/c) (fresh-var))))))

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

;;; What follows are some convenience macros to make programming with μKanren more natural

(define-syntax Zzz
  (syntax-rules ()
    [(_ g) (λ (s/c) (λ () (g s/c)))]))

(define-syntax conj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 gs ...) (conj (Zzz g0) (conj+ gs ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 gs ...) (disj (Zzz g0) (disj+ gs ...))]))

(define-syntax conde
  (syntax-rules ()
    [(_ (g gs ...) ...) (disj+ (conj+ g gs ...) ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g gs ...) (conj+ g gs ...)]
    [(_ (x xs ...) g gs ...)
     (call/fresh (λ (x) (fresh (xs ...) g gs ...)))]))

;;; What shape does a stream $ have?
;;;
;;;     Stream :: (() -> (cons ⊤ Stream)) | ()
;;;
;;; A stream is either null or a thunk that returns a cons consisting
;;; of a value and the rest of the stream

;;; Stream -> List interface
(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take n $)
  (if (zero? n) '()
      (let ([$ (pull $)])
        (cond
          [(null? $) '()]
          [else (cons (car $) (take (- n 1) (cdr $)))]))))

(define (take-all $)
  (let ([$ (pull $)])
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

;;; Reification utilities

(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))

(define (reify-state/1st-var s/c)
  ;; (pretty-print s/c)
  (let ([v (walk* (var 'var0) (car s/c))])
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (let ([n (reify-name (var-name v))])
         (cons (cons v n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(define (reify-name n)
  (if (symbol? n)
      n
      (string->symbol (string-append "_" "." (number->string n)))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [else v])))

(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    [(_ n (xs ...) g gs ...)
     (mK-reify (take n (call/empty-state
                        (fresh (xs ...) g gs ...))))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (xs ...) g gs ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (xs ...) g gs ...))))]))

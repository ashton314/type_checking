#lang racket/base

(require "kanren.rkt")
(require racket/hash racket/pretty (except-in racket/match ==) (only-in racket/list append-map))

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

(define (i v [label 'v])
  (printf "~a: " label)
  (pretty-print v)
  v)

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

(define (extend+replace term newterm Θ)
  ;; TODO: occurs check
  (hash-set! Θ term newterm)
  (hash-for-each
   Θ
   (λ (k v)
     (hash-set! Θ k (walk/sub v term newterm))))
  Θ)

(define (walk/sub sexpr old new)
  (cond
    [(equal? sexpr old) new]
    [(list? sexpr) (map (λ (i) (walk/sub i old new)) sexpr)]
    [else (match sexpr
            [(tc/= l r)
             (tc/= (walk/sub l old new) (walk/sub r old new))]
            [(tt/arrow dom rg)
             (tt/arrow (walk/sub dom old new)
                       (walk/sub rg old new))]
            [(tt/expr ex)
             (tt/expr (walk/sub ex old new))]
            [else sexpr])]))

(define (t/unify cs [subs (make-hash)])
  (match cs
    ['() subs]
    [(cons (tc/= l r) c/rest)
       (displayln "------unify/Θ------")
       (i cs 'cs)
       (i l 'left)
       (i r 'right)
       ;; (i subs 'Θ)
       (match l
         ;; Equal things unify
         [(? (λ (x)
               (or (equal? x r)
                   (and (tt/bool? x) (boolean? (tt/expr-expr r)))
                   (and (tt/num? x) (number? (tt/expr-expr r))))))
          (t/unify c/rest subs)]

         ;; Variables get subsituted
         ;; FIXME: circularity check
         [(or (tt/var _) (tt/expr _))
          (if (hash-has-key? subs l)
              (t/unify (cons (tc/= (hash-ref subs l) r) c/rest) subs)
              (t/unify (i (walk/sub cs l r) 'new-cs) (extend+replace l r subs)))]

         ;; Arrow type
         [(tt/arrow (list domain₁) range₁)
          (match r
            [(tt/arrow (list domain₂) range₂)
             (t/unify (append (list (tc/= domain₁ domain₂)
                                    (tc/= range₁ range₂))
                              cs) subs)])]

         ;; Failed to match
         [else (error 't/unify "Failed to unify types: ~a and ~a;~nsubs: ~a" l r subs)])]))

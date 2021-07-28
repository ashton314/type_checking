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

;; (gen-constraints my-progn)

;;; Make it so that numbers unify with their type
(define (my-eqv? a b)
  (or (and (tt/num? a) (number? b))
      (and (tt/num? b) (number? a))
      (and (tt/bool? b) (boolean? a))
      (and (tt/bool? b) (boolean? a))
      (eqv? a b)))

(equivalent? my-eqv?)

(define (fetch-or-instantiate mut-hash key)
  (hash-ref mut-hash key
            (λ () (let ([new-key (gensym)])
                    (hash-set! mut-hash key new-key)
                    new-key))))

(define (extend+replace term newterm Θ)
  ;; TODO: occurs check
  (hash-set! Θ term newterm)
  (hash-for-each
   Θ
   (λ (k v)
     (hash-set! Θ k (walk/sub v term newterm))))
  Θ)

(define (walk/sub sexpr old new)
  ;; (displayln "-----walk/sub-------")
  ;; (i sexpr 'sexpr)
  ;; (i old 'old)
  ;; (i new 'new)
  ;; (displayln "-----end walk/sub---")
  (cond
    [(equal? sexpr old) new]
    [(list? sexpr) (map (λ (i) (walk/sub i old new)) sexpr)]
    [else (match sexpr
            [(tt/arrow dom rg)
             (tt/arrow (walk/sub dom old new)
                       (walk/sub rg old new))]
            [(tt/expr ex)
             (tt/expr (walk/sub ex old new))]
            [else sexpr])]))

(define (unify/Θ constraints [Θ (make-hash)])
  (match constraints
    ['() Θ]
    [(cons (tc/= l r) c/rest)
     (begin
       (displayln "------unify/Θ------")
       (i constraints 'cs)
       (i l 'left)
       (i r 'right)
       (i Θ 'Θ)
       (match l
         [(tt/var varname)
          (if (hash-has-key? Θ l)
              (unify/Θ (cons (tc/= (hash-ref Θ l) r) c/rest) Θ)
              (unify/Θ c/rest (extend+replace l r Θ)))]
         [(tt/expr expr)
          (if (hash-has-key? Θ l)
              (unify/Θ (cons (tc/= (hash-ref Θ l) r) c/rest) Θ)
              (unify/Θ c/rest (extend+replace l r Θ)))]
         [(or (tt/num) (tt/bool))
          (cond
            [(or (equal? l r)
                 (and (tt/bool? l) (boolean? (tt/expr-expr r)))
                 (and (tt/num? l) (number? (tt/expr-expr r))))
             (unify/Θ c/rest Θ)]
            [else 
             (error (format "Expected ~a, got ~a" l r))])]
         [(tt/arrow (list d₁) r₁)
          (i 'hit)
          (match r
            [(tt/arrow (list d₂) r₂)
             (unify/Θ (cons (tc/= d₁ d₂)
                            (cons (tc/= r₁ r₂) constraints)) Θ)]
            [else (error (format "Expected arrow ~a, got ~a" l r))])]))]))

(define (fix-substitutions subs [last-subs null])
  (if (equal? subs last-subs)
      subs
      (let ([next-subs (hash-copy subs)])
        (hash-for-each
         subs
         (λ (k v)
           (hash-for-each
            subs
            (λ (k2 v2) (hash-set! next-subs k2 (walk/sub v2 k v))))))
        (fix-substitutions next-subs subs))))

;; (define the-subs (unify/Θ (gen-constraints '(λ (a) (+ 1 a))) (make-hash)))

;; (define (tt->kanren constraints [subs (make-hash)] [acc null])
;;   (if (null? constraints)
;;       (values acc subs)
;;       (match (car constraints)
;;         [(tc/= (tt/expr expr) type-or-var)
;;          (let ([expr-var (fetch-or-instantiate subs expr)])
;;            (match type-or-var
;;              [(tt/var var-name) ]))])))

;;; ap1 (tt/expr '(+ a 1))
;; (conde
;;  ;; (== a (tt/var 'a))
;;  (== 1 (tt/num))
;;  (== a (tt/num))
;;  (== 1 (tt/num))
;;  (== ap1 (tt/num))
;;  (== (tt/expr '(λ (a) (+ a 1))) (tt/arrow (list (tt/var 'a)) (tt/expr '(+ a 1))))
;;  (== (tt/expr 5) (tt/num))
;;  (== (tt/expr '(λ (a) (+ a 1))) (tt/arrow (list (tt/expr 5)) (tt/expr '((λ (a) (+ a 1)) 5)))))

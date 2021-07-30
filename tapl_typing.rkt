#lang racket/base

(require racket/hash racket/pretty
         (except-in racket/match ==)
         (only-in racket/list append-map))

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
(struct tt/string type-type () #:transparent)

(define (i v [label 'v])
  (printf "~a: " label)
  (pretty-print v)
  v)

(define (gen-constraints expr)
  (match expr
    [(? number?) (list (tc/= (tt/expr expr) (tt/num)))]
    [(? boolean?) (list (tc/= (tt/expr expr) (tt/bool)))]
    [(? symbol?) (list (tc/= (tt/expr expr) (tt/var expr)))]
    [(? string?) (list (tc/= (tt/expr expr) (tt/string)))]
    [`(concat ,s1 ,s2)
     (append (gen-constraints s1) (gen-constraints s2)
             (list (tc/= (tt/expr expr) (tt/string))
                   (tc/= (tt/expr s1) (tt/string))
                   (tc/= (tt/expr s2) (tt/string))))]
    [`(,(or 'substr? 'string<?) ,s1 ,s2)
     (append (gen-constraints s1) (gen-constraints s2)
             (list (tc/= (tt/expr expr) (tt/bool))
                   (tc/= (tt/expr s1) (tt/string))
                   (tc/= (tt/expr s2) (tt/string))))]
    [`(,(or '+ '- '* '/) ,lhs ,rhs) (append (gen-constraints lhs) (gen-constraints rhs)
                                            (list (tc/= (tt/expr lhs) (tt/num))
                                                  (tc/= (tt/expr rhs) (tt/num))
                                                  (tc/= (tt/expr expr) (tt/num))))]
    [`(zero? ,e) (append (gen-constraints e)
                         (list (tc/= (tt/expr expr) (tt/bool))))]
    [`(if ,c ,tc ,fc) (append (gen-constraints c) (gen-constraints tc) (gen-constraints fc)
                              (list (tc/= (tt/expr c) (tt/bool))
                                    (tc/= (tt/expr tc) (tt/expr fc))
                                    (tc/= (tt/expr expr) (tt/expr tc))))]
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
  ;; Funny; TaPL doesn't mention this part, but PLAI does. If I use
  ;; both, I get nicer results. Huh.
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
     (match (cons l r)
       ;; l = r (for some definition of equality)
       [(cons l r)
        #:when (or (equal? l r)
                   (and (tt/bool? l) (tt/expr? r) (boolean? (tt/expr-expr r)))
                   (and (tt/string? l) (tt/expr? r) (string? (tt/expr-expr r)))
                   (and (tt/num? l) (tt/expr? r) (number? (tt/expr-expr r))))
        (t/unify c/rest subs)]

       ;; l = X (where X is a variable)
       [(cons (or (tt/var _) (tt/expr _)) _)
        (if (hash-has-key? subs l)
              (t/unify (cons (tc/= (hash-ref subs l) r) c/rest) subs)
              (t/unify (walk/sub cs l r) (extend+replace l r subs)))]

       ;; r = X (where X is a variable)
       [(cons _ (or (tt/var _) (tt/expr _)))
        (if (hash-has-key? subs r)
              (t/unify (cons (tc/= (hash-ref subs r) l) c/rest) subs)
              (t/unify (walk/sub cs r l) (extend+replace r l subs)))]

       ;; arrow types: unify domain and range
       [(cons (tt/arrow (list domain₁) range₁) (tt/arrow (list domain₂) range₂))
        (t/unify (append (list (tc/= domain₁ domain₂)
                               (tc/= range₁ range₂))
                         cs) subs)]
       [else (error 't/unify "Failed to unify ~a and ~a with substitutions ~a" l r subs)])]))

(define (tc expr)
  (t/unify (gen-constraints expr)))

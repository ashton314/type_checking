#lang racket/base

(require racket/match)

(struct closure (params body ρk) #:transparent)

;;; Eval, but with holes in the expressions for type annotations to go
(define (→ e ρ)
  (match e
    [(? number?) e]
    [(? boolean?) e]
    [(? symbol?) (hash-ref e ρ)]
    [`(if _ ,c ,t ,f)
     (→ (if (→ c ρ) t f) ρ)]
    [`(λ _ (,xs ...) ,b)
     (closure xs b ρ)]
    [`(,ef _ ,es ...)
     (match (→ ef ρ)
       [(closure xs b ρk)
        (→ b (ext-env ρk xs (map (λ (a) (→ a ρ)) es)))])]))

(define (ext-env ρ xs vs)
  (foldl (λ (var val acc) (hash-set acc var val)) xs vs ρ))

(define (fresh-env)
  (make-immutable-hash
   (list (cons '+ (λ xs (apply + xs)))
         (cons '- (λ xs (apply - xs)))
         (cons '* (λ xs (apply * xs)))
         (cons '/ (λ xs (apply / xs)))
         (cons '= (λ xs (apply = xs)))
         )))


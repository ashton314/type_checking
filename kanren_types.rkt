#lang racket/base

(require racket/pretty "kanren.rkt")

(define (Γ-lookup Γ x τ)
  (disj (fresh (Γ*)
               (conj (== Γ (cons (cons x τ) Γ))))
        (fresh (Γ* x* τ*)
               (conj (== Γ (cons (cons x* τ*) Γ*))
                     (Γ-lookup Γ* x τ)))))

(define (Γ-extend Γ x τ Γ*)
  (== (cons (cons x τ) Γ) Γ*))

(define (tc e Γ τ)
  (conde [(fresh (n) (== e `(number ,n)))
          (== τ 'number)]
         [(fresh (b) (== e `(boolean ,b)))
          (== τ 'boolean)]
         [(fresh (v) (conj+ (== e `(var ,v))
                            (Γ-lookup Γ v τ)))]
         [(fresh (op lhs rhs lhs-τ rhs-τ)
                 (conj+
                  (== e `(,op ,lhs ,rhs))
                  (disj+ (== op '+) (== op '-) (== op '*) (== op '/))
                  (tc lhs Γ lhs-τ)
                  (tc rhs Γ rhs-τ)
                  (== lhs-τ 'number)
                  (== rhs-τ 'number)
                  (== τ 'number)))]
         [(fresh (a a-τ)
                 (conj+
                  (== e `(zero? ,a))
                  (tc a Γ a-τ)
                  (== τ 'boolean)))]
         [(fresh (c t-case f-case c-τ t-case-τ f-case-τ)
                 (conj+
                  (== e `(if ,c ,t-case ,f-case))
                  (tc c Γ c-τ)
                  (tc t-case Γ t-case-τ)
                  (tc f-case Γ f-case-τ)
                  (== c-τ 'boolean)
                  (== t-case-τ f-case-τ)
                  (== τ t-case-τ)))]))

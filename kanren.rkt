#lang racket/base

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

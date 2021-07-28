#lang racket/base

(require racket/pretty)

(require "kanren.rkt")

(define (father p s)
  (conde ((== p 'paul) (== s 'jason))
         ((== p 'john) (== s 'henry))
         ((== p 'jason) (== s 'tom))
         ((== p 'peter) (== s 'brian))
         ((== p 'tom) (== s 'peter))))

(define (grandfather g s)
  (fresh (p) (father g p) (father p s)))

;; (define (great-grandfather gg s)
;;   (fresh (f) (conj+ (grandfather gg f) (father f s))))

;; (define (great-grandfather2 gg s)
;;   (fresh (g) (conj+ (father gg g) (grandfather g s))))

;; Run to find out who's who's grandfather:
;; (run* (rel p s) (conj+ (grandfather p s) (== (cons p s) rel)))
;; => '((paul . tom) (jason . peter) (tom . brian))

;; (define (rev lst ret)
;;   (fresh (hd tl tmp)
;;           (conde ((== lst '()) (== ret '()))
;;                  ((== lst (list tmp)) (== ret (list tmp)))
;;                  ((== (cons hd tl) lst) (rev tl tmp) (== ret (append (list tmp) (list hd)))))))

;; ;; (run* (r) (rev '() r))
;; ;; (run* (r) (rev '(1) r))
;; (run* (r) (rev '(1 2 3) r))


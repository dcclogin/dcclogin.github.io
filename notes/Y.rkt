#lang lazy

(require lazy/force)


;; -------------- (x x) = objet a ----------------
;;
;; R := { x | ¬ (x ∈ x) }
;; R := λ x . ¬ (x x)
;; (R R) = ¬ (R R) = ¬¬ (R R) = ...
;; perpetuator of ⊥
;;
;; -----------------------------------------------


(define fix
  (λ (F) (F (fix F))))

(define Y
  (λ (F)
    ((λ (x) (F (x x)))
     (λ (x) (F (x x))))))


;; -------------- factorial ----------------

(define fact
  (λ (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n))))))


;; factF : (ℕ → ℕ) → (ℕ → ℕ)
(define factF
  (λ (fact)
    (λ (n)
      (if (zero? n)
          1
          (* n (fact (sub1 n)))))))

;; -------------- asymptotic ---------------

(define ⊥
  (λ (n) (error "contradiction.")))

(define fact₀
  (λ (n)
    (if (zero? n) 1 (⊥ n))))

(define fact₁
  (λ (n)
    (if (zero? n) 1 (* n (fact₀ (sub1 n))))))

(define fact₂
  (λ (n)
    (if (zero? n) 1 (* n (fact₁ (sub1 n))))))


(define (fact-gen m)
  (if (zero? m)
      fact₀
      (λ (n)
        (if (zero? n)
            1
            (* n ((fact-gen (sub1 m)) (sub1 n)))))))

((fact-gen 5) 5)


;; (F (F (F (... (F ⊥)))))
(define (Y-approx m)
  (λ (F)
    (if (zero? m)
        (F ⊥)
        (F ((Y-approx (sub1 m)) F)))))

(((Y-approx 5) factF) 5)


;; -------------- streams ---------------

(define S
  (λ (x)
    (cons x (S (add1 x)))))

(define SF
  (λ (F)
    (λ (x)
      (cons x (F (add1 x))))))

(define Sℕ (S 0))
(define SFℕ ((Y SF) 0))


(define factS
  (λ (n acc)
    (cons acc (factS (add1 n) (* n acc)))))

(define fibS
  (λ (a b)
    (cons a (fibS b (+ a b)))))


(define takeN
  (λ (s n)
    (if (zero? n)
        null
        (cons (! (car s))
              (! (takeN (cdr s) (sub1 n)))))))


((fix factF) 5)
((Y factF) 5)

(takeN Sℕ 10)
(takeN SFℕ 10)

(takeN (factS 1 1) 10)
(takeN (fibS 1 1) 10)

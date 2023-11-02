;;; Quelques exemples.                  -*- mode: lisp-data; coding: utf-8 -*-

2                                       ; ↝ 2
+                                       ; ↝ <function>
(+ 2 4)                                 ; ↝ 6

((λ x x) 2)                             ; ↝ 2

(((λ x (λ y (* x y)))
  3)
 5)                                     ; ↝ 15

(let true false
  (if true 1 2))                       ; ↝ 2

(ref! 5)                                ; ↝ ptr<0>

(let c1 (ref! 5)
 (let c2 c1
  (let _ (set! c2 6)
   (+ (get! c1) (get! c2)))))           ; ↝ 12

(let c1 (ref! 1)
 (let c2 (ref! 2)
  (let c3 (ref! 3)
   (let _ (set! c1 (+ 1 (get! c1)))
    (let _ (set! c2 (+ 1 (get! c2)))
     (let _ (set! c3 (+ 1 (get! c3)))
      (+ (get! c1) (+ (get! c2) (get! c3)))))))))  ; ↝ 9

(letrec ((c1 (ref! 1))
         (c2 (ref! 2))
         (c3 (ref! 3)))
  (+ (get! c1) (+ (get! c2) (get! c3)))); ↝ 6

(letrec ((a +)
         (s -))
  (letrec ((+ 1)
           (- 2))
    (a + -)))                           ; ↝ 3

(letrec ((odd  (λ x (if (= x 1) true (even (- x 1)))))
         (even (λ x (if (= x 0) true (odd (- x 1))))))
  (odd 7))                              ; ↝ True

(letrec ((fac (λ x (if (< x 2) x (* x (fac (- x 1)))))))
  (fac 5))                              ; ↝ 120
#lang sicp

;Exercise 1.1
;10
;12
;8
;3
; 6
;19
;#f
;4
; 16
;6
;16


;Exercise 1.2
;(/
; (+ 5 4
;    (- 2 (- 3 (+ 6 (/ 4 5)))))
; (* 3 (- 6 2) (- 2 7)))

;Exercise 1.3
(define (largest-sum-squares a b c)
  ;sum of a^2+b^2+c^2
  (+ (* a a) (* b b) (* c c)
     ;- (min a b c)^2
     (- (* (min a b c) (min a b c)))))

;Exercise 1.4
;What the procedure does is if b›d then we have a+b, otherwise a-b.

;Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;(test 0 (p))
;Given (test 0 (p))
;then would substitute to
;(if (= 0 0) 0 (p))
;For normal-order of evaluation we would have
;(if (= 0 0) 0 (p))
;to be fully expand to
;(if #t 0 (p))
;0
;While for applicative-order of evaluation we would have
;(if (= 0 0) 0 (p))
;into itsel recursively since it recursively evaluates its arguments


;Exercise 1.6
;It would compute the square root as the previous example.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x) x)))

;(define (sqrt x)
;  (sqrt-iter 1.0 x))

;Using the applicative order of evaluation
; recursively expand (sqrt-iter 1.0 9) to
; (sqrt-iter 1.0 9) to
; (new-if (good-enough? guess 9)
;guess
;(sqrt-iter (improve guess 9) 9)))
; then expand (sqrt-iter (improve guess 9) 9)
; repeatedly forever.

;Exercise 1.7
;given
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))


;(define (sqrt x)
;  (sqrt-iter 1.0 x))

;Hence we can improve it as follow
(define (new-good-enough? guess x)
  (< (abs (- (square guess) x)) (* 0.001 guess)))

(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x) x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

;Exercise 1.8
;Newton's method
(define (cube x)
  (* x (square x)))

(define (cube-average x y)
  (/ (+ (/ x (square y)) (* y 2)) 3))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) (* 0.001 guess)))

(define (cube-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-iter (cube-average x guess) x)))

(define (cube-root x)
  (cube-iter 1.0 x))

;Exercise 1.9
;given
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
;and 
;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

;For the first one we have
;(+ 4 5)
;(inc (+ (dec 4) 5))
;(inc (+ 3 5))
;(inc (inc (+ (dec 3) 5)))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ (dec 2) 5))))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
;This process is recursive as it expands and contracts, whuch isn't linear.

;For the second one we have
;(+ 4 5)
;(+ (dec 4) (inc 5))
;(+ 3 6)
;(+ (dec 3) (inc 6))
;(+ 2 7)
;(+ (dec 2) (inc 7))
;(+ 1 8)
;(+ (dec 1) (inc 8))
;(+ 0 9)
;9
;This process is linear as it remains mostly the same, and doesn't "peak" unlike the previous one.


;Exercise 1.10
;(A 1 10)

;(A (- 1 1) (A 1 (- 10 1)))
;(A 0 (A 1 9))
;(* 2 (A 1 9))
;(* 2 (A (- 1 1) (A 1 (- 9 1))))
;(* 2 (A 0 (A 1 8)))
;(* 2 (* 2 (A 1 8)))
;(* 2 (* 2 (A (- 1 1) (A 1 (- 8 1)))))
;(* 2 (* 2 (A 0 (A 1 7))))
;(* 2 (* 2 (* 2 (A 1 7))))
;(* 2 (* 2 (* 2 (A (- 1 1) (A 1 (- 7 1))))))
;(* 2 (* 2 (* 2 (A 0 (A 1 6)))))
;(* 2 (* 2 (* 2 (* 2 (A 1 6)))))
;(* 2 (* 2 (* 2 (* 2 (A (- 1 1) (A 1 (-6 1)))))))
;(* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (A 1 5))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (A (- 1 1) (A 1 (- 5 1))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 4)))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A (- 1 1) (A 1 (- 4 1)))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 3))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A (- 1 1) (A 1 (- 3 1))))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 2)))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A (- 1 1) (A 1 (- 2 1)))))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1))))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 32)))))
;(* 2 (* 2 (* 2 (* 2 64))))
;(* 2 (* 2 (* 2 128)))
;(* 2 (* 2 256))
;(* 2 512)
;1024

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))




;(A 1 10)
;1024
;(A 2 4)
;65536
;(A 3 3)
;65536


;Exercise 1.11
(define (recursive-f n)
    (if (< n 3)
        n
        (+ (recursive-f (- n 1)) (* 2 (recursive-f (- n 2))) (* 3 (recursive-f (- n 3))))))


(define (iterative-f n)
  (define (iter a b c count)
    (if (< count 3)
        a
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))
;(recursive-f 4) 
;(iterative-f 4)

;Exercise 1.12
(define (! n)
  (define (iter a b)
    (if (< b 1)
        a
        (iter (* a b) (- b 1))))
  (iter 1 n))

(define (C n r)
  (/ (! n)(* (! (- n r)) (! r))))


;Exercise 1.15
;(a) We have:
; (sin 12.15)
; (p (sin 4.05))
; (p (p (sin 1.35)))
; (p (p (p (sin 1.35))))
; (p (p (p (p (sin 0.45)))))
; (p (p (p (p (p (sin 0.15))))))
; (p (p (p (p (p (p (sin 0.05)))))))
; The procedure p has been called 5 times
;(b) Given sin a we have
; a/(3^n)< c steps,
; hence the order of growth is n log3 a, so phi(log n).
; Finally, space-wise the number of variables didn't increase so space remaind constant phi(1).

;Exercise 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;Exercise 1.17
(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b) (* 2 (fast-multi a (/ b 2))))
        (else (+ a (fast-multi a (- b 1))))))


;Exercise 1.18
(define (fast-multi-ver2 a b)
  (cond ((and (< a 0) (>= b 0)) (- (fast-multi (- a) b)))
        ((and (< b 0) (>= a 0)) (- (fast-multi a (- b))))
        ((and (< a 0) (< b 0)) (fast-multi (- a) (- b)))
        (else (fast-multi a b))))

;Exercise 1.19
(define (fast-fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))    ;compute p'
                   (+ (* 2 q p) (* q q))  ;compute q'
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))


;(fast-fib 21)

;Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

;Old implementation for finding a divisor.
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


;Begin new implementation for exercise 1.23.
;(define (next test-divisor)
;  (if (= test-divisor 2)
;      3
;      (+ test-divisor 2)))

;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (next test-divisor)))))
;End of implementation for exercise 1.23.

(define (divides? a b)
  (= (remainder b a) 0))

;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)

;Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes-iter i n)
  (cond ((<= i n)
         (timed-prime-test i)
         (search-for-primes-iter (+ i 2) n))))

(define (search-for-primes i n)
  (if (odd? i)
      (search-for-primes-iter i n)
      (search-for-primes-iter (+ i 1) n)
      ))
;(search-for-primes 1000 1090)
;(newline)
;(sqrt 10) ;10000

;(search-for-primes 10000 10090)
;(newline)
;(sqrt 100) ;100000

;(search-for-primes 100000 100090)
;(newline)
;(sqrt 1000) ;100000



;Answer: We have primes: 1009, 1013, 1019 which takes 1+2+1=4 units of runtime that approximates to sqrt(10) = 3.1622776601683795,
;and primes 10007, 10009, 10037 which takes 4+3+4=10 units of runtime that is exactly to sqrt(100)=10,
;and primes 100003, 100019, 100043 which takes about 11+10+11=32 units of runtime that approximates to sqrt(1000) = 31.622776601683793
;so is a relationship with the method sqrt(n) units of runtime for  each n*10000 digits computaed.


;1.23
(search-for-primes 10000000 10000200)
;Answer: For large numbers like 10000000 the total of the new procedure is 246 units of runtime with my modified change (next test-divisor), while its original one is 716 units of runtime (+ test-divisor 1).

;1.24
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))



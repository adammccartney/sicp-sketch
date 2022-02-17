#| rec-it-ogrow.scm: a bunch of sketches and exercises in and around linear
 | recursion, tree recursion, linear iteration and oders of growth. Lots of
 | references to various snippets and problems from SICP chapter 1 |#


#| this grows as fast as the size of the fib(n) itself |#
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))


(fib-rec 35)

#| linear iterative version of fib 
 | this grows linearly |#
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))


(fib 15)

(fib-rec 100)


#| Olog(n) |#
(define (fib-log n)
  (/ (- (fast-expt phi n) (fast-expt psi n)) (sqrt 5)))

(fib-log 5)

(fast-expt psi 2)

#| Exe 1.13: Prove that fib (n) is the closest integer to phi^n / sqrt(5),
 | where phi = (1 + sqrt(5))/2. Hint let psi = (1 - sqrt(5))/2. Use induction
 | to define the Fibonacci numbers to prove that: 
 | Fib(n) = (phi^n - psi^n)/sqrt(5) |# 

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
      (average guess (/ x guess)))
    (define (sqrt-iter guess)
      (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (phi)
  (/ (+ 1 (sqrt 5)) 2))

(define (psi)
  (/ (- 1 (sqrt 5)) 2))

#| linear recursive process, requires O(n) steps and O(n) space. |#
(define (exp b n)
  (if (= n 0)
    1
    (* b (exp b (- n 1)))))

#| same as a linear iterative process, require O(n) steps and O(1) space. |#
(define (exp b n)
  (exp-iter b n 1))

(define (exp-iter b counter product)
  (if (= counter 0)
    product
    (exp-iter b
              (- counter 1)
              (* b product))))

(RESTART 1)

#| use successive squaring: 
 | b^2 = (b^(n/2))^2  if n is even
 | b^n = b * b^(n-1)  if n is odd
 |#

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(/ 8 3)

(even? 64)

(exp 2 16)
(fast-expt 2 3)

#| Exercise 1.16: Design a procedure that evolves an iterative exponentiation process that uses successive squaring
 | and uses a logarithmic number of steps, as does fast-expt.
 | (Hint: Using the observation that (b^n/2)^2 = (b^2)^n/2, keep,
 | along with the exponent n and the base b, an additional
 | state variable a, and define the state transformation in such
 | a way that the product ab^n is unchanged from state to state.
 | At the beginning of the process a is taken to be 1, and the
 | answer is given by the value of a at the end of the process.
 | In general, the technique of defining an invariant quantity
 | that remains unchanged from state to state is a powerful
 | way to think about the design of iterative algorithms.)
 |#

(define (halve x)
  (/ x 2))

(define (exp-lt b n)
    (exp-iter-lt b n 1))

(define (exp-iter-lt b n a)
  (cond ((= n 0) 1)
        ((= n 1) (* b a))
        ((even? n) (exp-iter-lt (* b b) 
                                (/ n 2) 
                                a))
        (else (exp-iter-lt (* b b)
                           (/ (- n 1) 2)
                           b))))

(exp-lt 3 9)

#| Exercise 1.17: The exponentiation algorithms in this section are based on performing 
 | exponentiation by means of repeated multiplication. In a similar way, one can perform
 | integer multiplication by means of repeated addition. e
 | following multiplication procedure (in which it is assumed
 | that our language can only add, not multiply) is analogous to the expt
 | procedure: |#
 (define (* a b)
   (if (= b 0)
     0
     (+ a (* a (- b 1))))))

#|This algorithm takes a number of steps that is linear in b.
 |Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which
 |divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a
 |logarithmic number of steps. |#

(define (double b)
  (* b 2))
(define (halve b)
  (/ b 2))

(define (fast-mult a b)
  (cond ((even? b)
         (fast-mult (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

(fast-mult 24 66)

(half 10)

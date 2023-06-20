;; accounts
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Uknown request -- MAKE ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 100)

;; Exercise 3.1
#| An accumulator is a procedure that is called repeatedly with a single
 | numeric argument and accumulates its arguments into a sum. Each time it is
 | called, it returns the currently accumulated sum. Write a procedure
 | make-accumulator that generates accumulators, each maintining an independent
 | sum. The input to make-accumulator should specify the initial value of the
 | sum; for example
 ;;
 ;; (define A (make-accumulator 5))
 ;; 
 ;; (A 10)
 ;; 15
 ;; 
 ;; (A 10)
 ;; 
 ;; 25 |#
(define (make-accumulator sum)
  (define (accumulate incr)
    (set! sum (+ sum incr))
  sum)
  (define (dispatch m)
    (cond ((eq? m 'accumulate) accumulate)
          (else (error "Unknown request -- MAKE ACCUMULATOR"
                       m))))
  dispatch)

(define A (make-accumulator 10))

((A 'accumulate) 5)
((A 'accumulate) 10)


    #| accumulate |#
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

;; Exercise 3.2
#| In software-testing applications, it is useful to be able to count the 
 | number of times a given procedure is called during the course of a computation. 
 | Write a procedure make-monitored that takes as input a procedure, f, that itself 
 | takes one input. The result returned by make-monitored is a third procedure, say 
 | mf, that keeps track of the number of times it has been called by maintaining an 
 | internal counter. If the input to mf is the special symbol how-many-calls?, then 
 | mf returns the value of the counter. If the input is the special symbol reset-count, 
 | then mf resets the counter to zero. For any other input, mf returns the result of 
 | calling f on that input and increments the counter. For instance, we could make a 
 | monitored version of the sqrt procedure:

 ;;     (define s (make-monitored sqrt))

 ;;     (s 100)
 ;;     10

 ;;     (s 'how-many-calls?)
 ;;     1
  |#

(define (sqrt x)
  (define (square x) 
    (* x x))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (mf sum)
  (set! sum (+ sum 1))
  sum)

(mf 0)

(define (make-monitored f)
  (define sum 0)
  (define (mf count)
      (set! sum (+ count 1))
    sum)
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (mf sum))
            ((number? m) (f m))
            (else (error "Unknown request -- MAKE MONITORED"
                         m))))
    dispatch)

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))


(define (sequencer a b)
 (define (adder a b)
   (+ a b))
 (define (subtractor a b)
   (- a b))
 (and (display (subtractor a b))
      (adder a b)))

(sequencer 10 20)




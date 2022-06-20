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




(+ 2 3 4)
(define something 4)

(define smallprimes (list 2 5 7))

(cons 2 smallprimes)

(define sepisfavs 
  (list "lollypops" "rainbows" "puppies"))
(cons "ice-cream" sepisfavs)

(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))

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

(square 4)
(sqrt 2)
(sqrt 4)

#| Define a procedure to compute all elements of Pascal's triangle 
 | by means of a recursive process
 | assumes exponent is operating on (x + y) 
 | (x + y)^0 = 1
 | (x + y)^1 = x + y 
 |#


(define (makelevel n)
  (if (= n 0)
    (list 1)
    (cons n (makelevel (- n 1)))))

(makelevel 5)

(define (tri-pascal levels)
  (define item 0)
  (cond ((= levels 0) 1)
        (define (level-iter) 
          (tripascal (- level 1)))))

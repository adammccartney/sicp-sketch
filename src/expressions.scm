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

(define (len lst)
  (cond ((null? lst) 0)
        (else (+ 1 (len (cdr lst))))))

(len (list 1 2 1))

(define (makelist n)
  (if (= n 0) 
    (list)
    (cons 1 (makelist (- n 1)))))

(makelist 5)


#| very simple recursion |#
(define (minusone n)
  (cond ((= n 0) 1)
        (else (display n)
          (minusone (- n 1)))))

(minusone 5)

(cons 1 (+ (car (list 1 2 1)) (car (cdr (list 1 2 1)))))


#| add an element to a list |#
(define (extend l . xs)
  (if (null? l)
    xs
    (cons (car l) (apply extend (cdr l) xs))))

(extend '(1 2) 1)

#| adds the first and second elements in a list |#
(define (sumpair lst)
  (cond ((= (len lst) 1) 1)
        (else (+ (car lst) (car (cdr lst))))))

(sumpair (list 1 3 6 3 1))

(define (len lst)
  (cond ((null? lst) 0)
        (else (+ 1 (len (cdr lst))))))

(define (sumpair-iter lst newlist)
  (define (sumpair lst)
    (cond ((= (len lst) 1) 1)
          (else (+ (car lst) (car (cdr lst))))))
    (define (len lst)
      (cond ((null? lst) 0)
            (else (+ 1 (len (cdr lst))))))
    (define (extend l . xs)
      (if (null? l)
        xs
        (cons (car l) (apply extend (cdr l) xs))))
  (cond ((= (len lst) 1)   #| last element, append one |#
        (extend newlist 1))
        (else
          (sumpair-iter (cdr lst) (extend newlist (sumpair lst))))))

#| test |#
(sumpair-iter (list 1 2 1) (list 1 1))

(define (getlist lst)
  (define (len lst)
    (cond ((null? lst) 0)
          (else (+ 1 (len (cdr lst))))))
  (if (= (len lst) 1) 
    (list 1 1))
    (list 1))

#| need to define a way to create a new list of length len + 1 with all ones
 | then iterate over this list and replace the middle values with ones
 | calculated from a previous list 
 |# 

getlist (list 1 1 1 1 1 1))

(makelist (list 1))

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

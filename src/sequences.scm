#| filter |#
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5 6))

#| accumulate |#
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))



(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


(fib 10)

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(even-fibs 50)


#| Exercise 2.33
 | fill in the missing expressions to complete the following definitions of
 | some basic list-manipulation operations as accumulations:
 |#

#| map |#
(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(mymap square (list 1 2 3 4 5))

#| remember lambda |#
(define f
  (lambda (x y) (+ (square x) y)))

(f 4 3)

#| append |#
(define (myappend seq1 seq2)
  (accumulate cons seq2 seq1))

(myappend (list 1 2 3) (list 4 5 6))

(append (list 1 2 3) (list 4 5 6))

#| length |#

(length (list 1 2 3 4 5))

(define one-er 
  (lambda (x) (if (not (null? x)) 1)))

(one-er (car (list 1 2)))

(car (list 1 2))

(define (mylength sequence)
  (accumulate (lambda (x y) (+ (if (not (null? x)) y) 1)) 0 sequence))

#| without the mega crazy one-er |#
(define (shlength sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))



(define (testlength sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(testlength (list 1 2 3 4 5 6))

(shlength (list 1 2 3 4 5 6))

(mylength (list 1 2 3 4 5 6))

(define increment
  (lambda (x) (+ x 1)))

(increment 5)


#| Exercise 2.34.  Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. 
 | We evaluate the polynomial using a well-known algorithm called Horner's rule, which structures the computation as
 |
 | In other words, we start with an, multiply by x, add an-1, multiply by x, and
 | so on, until we reach a0. Fill in the following template to produce a procedure 
 | that evaluates a polynomial using Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence, from a0 through an.
 | 
 | (define (horner-eval x coefficient-sequence)
 |  (accumulate (lambda (this-coeff higher-terms) <??>)
 |             0
 |             coefficient-sequence))
 |
 | For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate
 | 
 | (horner-eval 2 (list 1 3 0 5 0 1))
 |#

(define aseq (list 1 3 0 5 0 1))

#| accumulate |#
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))



(define (horner-step coeff sequence)
  (+ (car sequence) (* (car (cdr sequence)) coeff)))

(horner-step 2 aseq)

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                  (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

#| Exercise 2.35
 | Redefine count-leaves from section 2.22 as an accumulation 
 | (define (count-leaves t)
     (accumulate (??) (??) (map (??) (??)))) |#

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define alist (list (list 1 2) (list 3 4) (list 5 6)))
(count-leaves alist)

(define (fringe x)
  (define nil '())
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))  #| trick is to test if node is a pair |#
        (else (append (fringe (car x)) (fringe (cdr x))))))

#| this would get us the count of leaves 
 | Step one, flatten the tree |#
(define (count-leaves-fringe t)
  (length (fringe t)))

(count-leaves-fringe alist)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

#| then, simply map a one onto each leaf in the flattened tree |#
#| the operator accumulates an addition of all these ones |#

(define (count-leaves-acc t)
  (accumulate + 
              0
              (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves-acc alist)
(enumerate-tree alist)

(map (lambda (x) 1) (enumerate-tree alist)) 

#| Exercise 2.36
 | The procedure accumulate-n is similar to accumulate except that it takes as 
 | its third argument a sequence of sequences, which are all assumed to have 
 | the same number of elements. It applies the designated accumulation procedure
 | to combine all the first elements of the sequences, all the second elements of 
 | the sequences, and so on, and returns a sequence of the results. For
 | instance, if s is a sequence containing four sequences, 
 | ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) 
 | should be the sequence (22 26 30). Fill in the missing expressions in the 
 | following definition of accumulate-n:
 | 
 | (define (accumulate-n op init seqs)
 |  (if (null? (car seqs))
 |      nil
 |      (cons (accumulate op init <??>)
 |          (accumulate-n op init <??>))))
 |#

(define (accumulate-n op init seqs)
  (define nil '())
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
          (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

#| step one: accumulate of the cars of the lists |#

(define multi-list (list (list 1 2 3) 
                         (list 4 5 6)
                         (list 7 8 9)
                         (list 10 11 12)))

(enumerate-tree multi-list)

(accumulate-n + 0 multi-list)

#| this first accumulate is working fine |#
(accumulate + 0 (map (lambda (x) (car x)) multi-list))


(map (lambda (x) (cdr x)) multi-list)

#| Exercise 2.37.  Suppose we represent vectors v = (vi) as sequences of numbers, 
 | and matrices m = (mij) as sequences of vectors (the rows of the matrix). 
 | For example, the matrix

 | [[1, 2, 3, 4],
 |  [4, 5, 6, 6],
 |  [6, 7, 8, 9]]

 | is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). 
 | With this representation, we can use sequence operations to concisely 
 | express the basic matrix and vector operations. 
 | These operations (which are described in any book on matrix algebra) are the following:


 | We can define the dot product as17

    (define (dot-product v w)
      (accumulate + 0 (map * v w)))

 | Fill in the missing expressions in the following procedures for computing 
 | the other matrix operations. (The procedure accumulate-n is defined in exercise 2.36.)

    (define (matrix-*-vector m v)
      (map <??> m))
    (define (transpose mat)
      (accumulate-n <??> <??> mat))
    (define (matrix-*-matrix m n)
      (let ((cols (transpose n)))
        (map <??> m)))
 |#

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(dot-product (select-row matrix 0) (select-column matrix 1))

#| select the row n from matrix m |#
(define (select-column matrix n)
  (map (lambda (x) (list-ref x n)) matrix))

(define (select-row matrix n)
  (list-ref matrix n))

(select-column matrix 2)
(select-row matrix 0)


#| matrix by a vector 
 | [[1, 1],     [[1], 
    [2, -1]]  *  [2]] 

 = [3, 0] 
|#
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))


(define small-m (list (list 1 1) (list 2 -1)))
(define vec (select-column small-m 0))

(matrix-*-vector small-m vec)

(lambda (x) (accumulate * 1 x)) 









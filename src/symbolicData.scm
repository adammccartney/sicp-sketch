(list 'a 'b 'c)

(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(list (list '(a b c))

(cadr '((x1 x2) (y1 y2)))
(memq 'red '((red shoes) (blue socks)))

(memq '((red shoes)) '((red shoes) (blue socks)))

#| memq returns false if item is present in list, otherwise it returns sublist
 | starting with the item. |#

(memq 'apples '(bananas oranges apples pears))
(memq 'apples '(bananas oranges apples pears))


(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        ((exponentiation? expr)
         (make-product 
           (make-product 
             (exponent expr)
             (make-exponentiation (base expr)
                                  (make-sum (exponent expr) -1)))
           (deriv (base expr) var)))
        (else
          (error "unknown expression type -- DERIV" expr))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((=number? e 1) e)
        ((and (number? b) (number? e)) (fast-expt b e))
        (else (list '** b e))))

#| tests |#
(make-exponentiation 'x 4)
(make-exponentiation 4 4)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


#| Exercise 2.56: Show how to extend the basic differentiator to handle more
 | kinds of expressions. For instance, implement the differentiation rule
 | d(u^n) / dx = nu^n-1(du/dx)
 | by adding a new clause to the deriv program and defining the appropriate
 | procedure exponentiation?, base, exponent, and make-exponentiation. (You may
 | use the symbol ** to denote the exponentiation.) Build the rules that
 | anything raised to the power 0 is 1 and anything raised to the power 1 is
 | the thing itself.
 |#

#| tests |#
(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(** x 2) 'x)


#| Unordered Sets |#

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((adjoin-set (car set1) set2))
        (else (union-set (cdr set1) set2))))

#| Exercise 2.59 implement the union-set operation for the unordered-list
 | representation of sets |#


#| tests |#

(element-of-set? 'a '(a b c)) 
(adjoin-set 'd '(a b c))

(intersection-set '(a b c) '(c d e))

(union-set '(a b c) '(c d e))


(> 4 5)


#| Ordered Sets |#

#| by ordering the set, we should expect to search the set n/2 times. This
 | means the growth is still O(n) but it saves us a factor of 2 number of steps
 | over the previous implmentation. |#

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


#| originally, with the unordered representation, we were running this
 | procedure in O(n^2) steps, because we performed a complete scan of set2 per
 | element of set1. 
 |#
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

#| Exercise 2.61 Give an implementation of adjoin-set using the ordered
 | representation. By analogy with element-of-set? show how to take advantage
 | of the ordering to produce a procedure that requires on the average about
 | half as many steps as with the unordered representation. |#

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 2 '(4 5 6))

#| Exercise 2.62 Give a O(n) implementation of union-set for sets represented
 | as ordered lists |#

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else union-set (cdr set1) (adjoin-set (car set1) set2))))

(union-set '(1 2 3) '(2 3 4))


(define (union-set set1 set2
  (cond ((and (null? set1) (null? set2)) '())
        ((adjoin-set (car set1) set2))
        (else (union-set (cdr set1) set2))))


#| sets as binary trees |#

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree-list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))


#| list to tree |#

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)  ; if length is none, we've reached the limit
                     ; return elements that have been collected
    (let ((left-size (quotient (- n 1) 2))) ; floor div of set len
      (let ((left-result (partial-tree elts left-size))) ; recurs def leftmost
        (let ((left-tree (car left-result))  ; left-tree is all but root
              (non-left-elts (cdr left-result)) ; is all but root and left-tree 
              (right-size (- n (+ left-size 1)))) ; return decr floor div 
          (let ((this-entry (car non-left-elts)) ; this-entry is root
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size))) ; recursive descent
                                                          ; for right path
            (let ((right-tree (car right-result)) 
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(list->tree '(1 3 5 7 9 11)) 
; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

#| Exercise 2.64 The preceding procedure list->tree converts an ordered list to
 | a balanced binary tree. The helper procedure partial-tree takes as arguments
 | an integer n and list of at least n elements and constructs a balanced tree
 | containing the first n elements of the list. The result returned by
 | partial-tree is a pair (formed with cons) whose car is the constructed tree
 | and whose cdr is the list of elements not included in the tree. 
 | a.) Write a short paragraph explaining as clearly as you can how
 | partial-tree works. Draw the tree produced by list->tree for the list 
 | (1 3 5 7 9 11)
 |
 |           5
 |        /     \
 |       1       9
 |      / \     / \
 |         3   7   11
 |        / \ / \  / \
 | description:
 | The procedure first performs a recursive descent on the leftmost branch,
 | returning to fill out the right branch when it encounters the empty list at
 | the end of the path to the left.
                 

#| Exercise 2.21 
 | The procedure square-list takes a list of numbers as arguments and returns a
 | list of the squares of those numbers.
 | (square.list (list 1 2 3 4))
 | (1 4 9 16)
 | Here are two definitions of square-list
 |#


(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

(define ulist (list 1 2 3 4))

(square-list ulist)

(+ 1 2)


(define mylist (list 1 2 3 4 5))

(car mylist)
(cdr mylist)




#| Exercise 2.23
 | The procedure for-each is similar to map. It takes as arguments the
 | procedure and a list of elements. However, rather than formatting a list of
 | the results, for-each just applies the procuedure to each of the elements
 | in turn, from left to right. The value returned by applying the procedure to
 | the elements are not used at all -- for-each is used with prcedures that
 | perform an action, such as printing. For example:
 | (for-each (lambda (x) (newline) (display x))
 |           (list 57 321 88))
 | 57
 | 321
 | 88
 | The value returned by the call to for-each (not illustrated above) can be
 | something arbitrar, such as true. Give an implementation of for-each.
 |#

(define (for-each proc items)
  (if (null? items)
    '()
    (and (proc (car items)) 
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


#| Exercise 2.25 
 | Give combinations of cars and cdrs that will pick 7 from each of the
 | following lists
 |#

(define odd (list 1 3 (list 5 7) 9))
(define seven (list (list 7)))
(define sixlists (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))


(cdr 
  (car 
    (cdr (cdr odd))))

(car seven)

(cdr 
  (car 
    (cdr 
      (car 
        (cdr 
          (car 
            (cdr 
              (car 
                (cdr 
                  (car 
                    (cdr sixlists)))))))))))


#| Exercise 2.26 
 | Suppose we define x and y  to be two lists:
 | what rexult is printed by the interpreter given the expressions that follow 
 |#

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)

(cons x y)

(list x y)

#| Exercise 2.18
 | Define a procedure reverse that takes a list as arugment and returns a list
 | of the same elements in reverse order:
 | (reverse (list 1 4 9 16 25))
 | (25 16 9 4 1)
 |#

(define alist (list ))

(display alist)

(define items (list 1 2 3 4 5))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(length items)

(define rlist (list))

(append (list (append rlist (list-ref items (- (length items) 1)))) (cdr
                                                                      items))

(cdr items)


(define (reverse items) 
  (define nil '())
  (define (iter items result) 
    (if (null? items) 
        result 
        (iter (cdr items) (cons (car items) result)))) 
  (iter items nil)) 

(define squares (list 1 4 9 16 25))

(reverse squares)


#| Exercise 2.27
 | Modify your reverse procedure of exercise 2.18 to produce a deep-reverse
 | procedure that takes a list as argument and returns as its value the list
 | with its elements reversed and with all sublists deep-reversed as well. For
 | example:
 | (define x (list (list 1 2) (list 3 4)))
 | x 
 | ((1 2) (3 4))
 | (reverse x)
 | ((3 4) (1 2))
 | (deep-reverse x)
 | ((4 3) (2 1))
 |#

(define alist (list (list 1 2) (list 3 4)))
(reverse alist)
(deep-reverse alist)

(cdr alist)

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

#| this deep-reverse works for lists with two sublists |#
(define (deep-reverse items)
    (define (reverse items) 
      (define nil '())
      (define (iter items result) 
        (if (null? items) 
            result 
            (iter (cdr items) (cons (car items) result)))) 
      (iter items nil)) 
  (define (rev-in-place alist)  #| this assumes that alist is a pair of lists |#
    (define (iter-rev alist result)
    (cons (reverse (car (cdr alist))) (cons (reverse (car alist)) '())))
  (reverse (rev-in-place (reverse items))))


#| here is a version that uses map |#
(define (deep-reverse items)
  (define (reverse items) 
        (define nil '())
        (define (iter items result) 
          (if (null? items) 
              result 
              (iter (cdr items) (cons (car items) result)))) 
        (iter items nil)) 
  (reverse (map reverse items)))

(define alist (list (list 1 2) (list 3 4) (list 5 6)))
(define blist (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(deep-reverse alist)
(deep-reverse blist)


(define (rev-lnode l)
  (reverse (car (cdr l))))

(define (rev-rnode l)
  (reverse (car l)

(if (list? (cdr l))
    (rev-node l)
    l)))

(define clist (cdr (list 1 2 3)))
  (reverse (car (cdr l))))

(define clist (list (list 1 2 3) (list 4 5) (list 6 7)))

(define blist (list 1 2))

(check-list blist)

(deep-reverse clist)
(deep-reverse alist)


(rev-in-place (list (list 1 2) (list 3 4)))

(check-list alist)
(check-list blist)

(deep-reverse alist)


(define (nil? alist)
  (not (atom? alist)))

(null? '())

(atom? '())


(car '())

(define alist (list (list 1 2) (list 3 4)))


(null? (cdr (cdr alist)))


(define dlist (list (list 2 3) (list 4 5) (list 6 7) (list 8 9)))

(cons (reverse (car (cdr dlist))) (cons (reverse (car dlist)) '()))


(count-nodes dlist)
(cdr dlist)

(cdr (cdr alist))

(pair? alist)

#| Exercise 2.28: Write a procedure fringe that takes as argument 
 | a tree (represented as a list) and returns a list whose
 | elements are all the leaves of the tree arranged in le-toright order. 
 | For example,
 | (define x (list (list 1 2) (list 3 4)))
 | (fringe x)
 | (1 2 3 4)
 | (fringe (list x x))
 | (1 2 3 4 1 2 3 4)
 |#

(define (fringe x)
  (define nil '())
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))  #| trick is to test if node is a pair |#
        (else (append (fringe (car x)) (fringe (cdr x))))))

(fringe (list 1 2 3 4 5 6))

(fringe alist)

(if (not (pair? (car items)))
  (car items))



#| Exercise 2.29 A binary mobile consists of two branches,
 | a le branch and a right branch. Each branch is a rod of
 | a certain length, from which hangs either a weight or another binary mobile. 
 | We can represent a binary mobile using compound data by constructing it from two branches
 | (for example, using list):
 | (define (make-mobile left right)
 | (list left right))
 | A branch is constructed from a length (which must be a
 | number) together with a structure, which may be either a
 | number (representing a simple weight) or another mobile:
 | (define (make-branch length structure)
 | (list length structure))
 |
 | a. Write the corresponding selectors left-branch and
 | right-branch, which return the branches of a mobile,
 | and branch-length and branch-structure, which return the components of a branch.
 |
 | b. Using your selectors, define a procedure total-weight
 | that returns the total weight of a mobile.
 |
 | c. A mobile is said to be balanced if the torque applied by
 | its top-le branch is equal to that applied by its top
 | right branch (that is, if the length of the le rod multiplied by the weight hanging from that rod is equal
 | to the corresponding product for the right side) and if
 | each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary
 | mobile is balanced.
 |
 | d. Suppose we change the representation of mobiles so
 | that the constructors are
 | (define (make-mobile left right) (cons left right))
 | (define (make-branch length structure)
 | (cons length structure))
 | How much do you need to change your programs to
 | convert to the new representation?
 |#

#| constructors |#
(define (make-mobile left right)
  (list left right))

(define (make-branch len struct)
  (list len struct))


#| selectors |#
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (reverse mobile)))

(define (branch-weight branch)
  (car (reverse branch)))

#| selects the vector (torque) of a branch |#
(define (vec branch)
  (* (car branch) (branch-weight branch)))

(vec abranch)

(define abranch (make-branch 10 15))
(define bbranch (make-branch 5 6))

(* (car abranch) (branch-weight abranch))

(define mymobile (make-mobile abranch bbranch))

(left-branch mymobile)
(right-branch mymobile)

#| define procedure total-weight |#
(define (total-weight mobile)
  (define (iter mobile sum)
    (if (null? mobile) 
      sum
      (iter (cdr mobile) (+ sum (branch-weight (car mobile))))))
    (iter mobile 0))

(total-weight mymobile)


#| check if mobile is balanced |#
(define (balanced? mobile)
  (define lvec (vec (left-branch mobile)))
  (define rvec (vec (right-branch mobile)))
  (equal? lvec rvec))

(define lbranch (make-branch 4 6))
(define rbranch (make-branch 8 3))
(define bmob (make-mobile lbranch rbranch))

(balanced? mymobile)
(balanced? bmob)

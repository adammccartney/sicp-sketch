;; some notes from the scheme book
;; https://scheme.com/tspl4/start.html#./start:h4
;; examples below are tested with guile-scheme

#| Let expressions retain their binding within the local scope only. 
 | This means that if other variables are referenced within the scope, 
 | they will seek to derive their assigned value from the parent scope.
 | This is commonly known as shadowing. |#

(define shadowy
  (let ([x 'a] [y 'b])
    (list (let ([x 'c]) (cons x y))
          (let ([y 'd]) (cons x y)))))

(define clarity
  (let ([x 'a] [y 'b])
    (list (let ([new-x 'c]) (cons new-x y))
          (let ([new-y 'd]) (cons x new-y)))))

;; show that these two lists are equivalent
(equal? shadowy clarity) ;; => #t


;; some further explanation of cond
(define shorter
  (lambda (l1 l2)
  (cond
   [(> (length l1) (length l2)) l2]
   [(< (length l1) (length l2)) l1]
    [else l1])))

;; some other routines on flattening lists etc.
(define (fringe x)
  (define nil '())
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))  #| trick is to test if node is a pair |#
        (else (append (fringe (car x)) (fringe (cdr x))))))

;; alternate definition is to explicitly use lambda
(define fringe
  (lambda (tree)
    (define nil '())
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree)) (fringe (cdr tree)))))))

#| this would get us the count of leaves 
 | Step one, flatten the tree |#
(define (count-leaves-fringe t)
  (length (fringe t)))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (list-compare l1 l2)
  (if (and (null? l1) (null? l2))
      #t  ;; list equivalence
      #f) ;; no list equivalence
  (if (= (length l1) (length l2))
      (if (eqv? (car l1) (car l2))
          (list-compare (cdr l1) (cdr l2))
          #f) ;; found items not equivalent
      #f)) ;; lists differ in length

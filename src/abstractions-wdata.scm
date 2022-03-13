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


#| Exercise 2.23
 | The procedure for-each is similar to map. It takes as arguments the
 | procedure and a list of elements. However, rather than formatting a list of
 | the results, for-each just applires the procuedure to each of the elements
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

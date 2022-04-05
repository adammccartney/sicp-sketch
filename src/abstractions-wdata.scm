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

(define nil '())

(define (reverse items) 
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

(cdr alist)

(define (list? x)
  (and (pair? x)
       (not (null? x))))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (if (list? (car items))
        (iter (cdr items) (cons (reverse (car items)) result))
        (iter (cdr items) (const (car items)) result))))
  (deep-reverse items nil))

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (display items))))
    

(list? (car alist))

(deep-reverse alist)
(deep-reverse squares)



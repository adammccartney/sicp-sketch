(define (qpop alist) #| for loop |#
  (define (iter current items)
    (if (null? items)
      current
        (iter (car items) (cdr items))))
  (iter (car alist) (cdr alist)))

(qpop alist)

(define alist (list 1 2 3 4 5))

(length (list))

(display (car alist))

(car alist)

(null? '())

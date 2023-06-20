(define job (lambda (x) x))

(call/cc
 (lambda (k)
   (* 5 4)))


(call/cc
 (lambda (k)
   (* 5 (k 4))))

(+ 2
   (call/cc
    (lambda (k)
           (* 5 (k 4)))))


;; light-weight process mechanism from tspl4
(define lwp-list '())
(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk)))))

(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p))))

(define pause
  (lambda ()
    (call/cc
      (lambda (k)
        (lwp (lambda () (k #f)))
        (start)))))

(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
(start)


;; Exercise 3.3.1
#| Use call/cc to write a program that loops indefinitely, printing a sequence of numbers
 | beginning at zero. Do not use any recursive procedures, and do not use any assignments. |#


(define incr
  (lambda (v)
    (display (+ v 1)) (newline) (+ v 1)))


(lwp (lambda () (let f () (pause) (incr 0) (f))))
(start)

(define (foo)
  (let ([x (call/cc
            (lambda (cont)
              (display "captured continuation\n")
              (cont 5)
              (display "continuation called\n")))])
    (display "returning x\n")
    x))


(define (incr y)
  (let ([x (call/cc
            (lambda (k)
              (display "incrementing y\n")
              (k (+ y 1))
              (display "assigned incremented y to x\n")))])
    (display (format #t "x: ~a\n" x))
    x))



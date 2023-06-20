;;
(define quadratic-formula
  (lambda (a b c)
    (let ([minusb (- 0 b)]
          [radical (sqrt (- (* b b) (* 4 (* a c))))]
          [divisor (* 2 a)])
      (let ([root1 (/ (+ minusb radical) divisor)]
            [root2 (/ (- minusb radical) divisor)])
        (cons root1 root2)))))

;; make a stack
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
         [(eqv? msg 'empty?) (null? ls)]
         [(eqv? msg 'push!) (set! ls (cons (car args) ls))]
         [(eqv? msg 'top) (car ls)]
         [(eqv? msg 'pop!) (set! ls (cdr ls))]
         [else "oops"])))))


;; make a queue
(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))

(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))

(define getq
  (lambda (q)
    (car (car q))))

(define delq!
  (lambda (q)
    (set-car! q (cdr (car q)))))


;; list-copy

(define list-copy
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (car ls)
              (list-copy (cdr ls))))))

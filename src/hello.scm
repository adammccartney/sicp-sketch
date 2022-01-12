(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? (quote ()))

(+ 2 2)

(define a 3)
(define b (+ a 1))
(+ a b (* a b))

t;; mostly snippets copied from scheme programming book

;; to copy a list
(define list-copy
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (car ls)
              (list-copy (cdr ls))))))


;; recursive functions can have more than one base case 
(define memv
  (lambda (x ls)
    (cond
     [(null? ls) #f]
     [(eqv? (car ls) x) ls]
     [else (memv x (cdr ls))])))

;; remember that scheme objects except for #f evaluate to #t
(memv 'a '(a b b d)) ;;  (a b b d)
(memv 'b '(a b b d)) ;;  (b b d)
(memv 'c '(a b b d)) ;;  #f
(memv 'd '(a b b d)) ;;  (d)
(if (memv 'b '(a b b d))
    "yes"
    "no") ;;  "yes"


#| how I stopped worrying and learned to love cons
 | cons will construct a pair
 | (cons 'a 'b) => (a . b)
 | a pair is a good natural representation of a tree |#

(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy (car tr))
              (tree-copy (cdr tr))))))

#| Ex 2.2: Consider the problem of representing line segments in a plane. Each
 | segment is represented as a pair of points: a starting point and an ending
 | point. Define a constructor make-segment and selectors start-segment and
 | end-segment that define the representation of segments in terms of points.
 | Furthermore, a point can be represented as a pair of numbers: the x
 | coordinate and the y coordinate. Accordingly, specify a constructor
 | make-point and selectors x-point and y-point that define this
 | representation. Finally, using your selectors and constructors, define a
 | procedure midpoint-segment that takes a line segment as argument and returns
 | its midpoint (the point whose coordinates are the average of the coordinates
 | of the endpoints). To try your procedures, you'll need a way to print
 | points:
 | 
 |   (define (print-point p)
 |     (newline)
 |     (display "(")
 |     (display (x-point p))
 |     (display ",")
 |      (display (y-point p))
 |      (display ")"))
 |#

#| constructor |#

(define (make-segment point-a point-b)
  (cons point-a point-b))

#| selectors |#
#| returns a point |#
(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

#| primitive constructor |#
(define (make-point x y)
  (cons x y))

#| primitive selector |#
(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

#| display a point |#
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
   (display (y-point p))
   (display ")"))


#| midpoint |#
(define (midpoint-segment segment)
  (define (mid-point a b)
    (make-point
        (/ (+ (x-point a) (x-point b))
           2)
        (/ (+ (y-point a) (y-point b))
           2)))
  (mid-point (start-segment segment)
               (end-segment segment)))


#| tests |#
(define home (make-point 1 2))

(x-point home)
(y-point home)

(define point-a (make-point 1 1))
(define point-b (make-point 4 4))

(mid-point point-a point-b)

(define segment-a (make-segment point-a point-b))

(start-segment segment-a)
(end-segment segment-a)
(midpoint-segment segment-a)

(define middle (midpoint-segment segment-a))
(print-point segment-a)



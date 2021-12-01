                           ;----------------------15.1---------------------------------
(display "15.1\n\n")

; FUNCTION
(define (f x)
  (- (+ (exp x) (* 2 x)) 26)
  )


;FIND SOLUTION
;---------------------------------------------------------------
(define chord ( lambda (a b epsilon) 
  (if (< (abs (- a b)) epsilon)
         b

         (begin505
           (set! a (- b (* (- b a) (/ (f b) (- (f b) (f a))))))
           (set! b (- a (* (- a b) (/ (f a) (- (f a) (f b))))))
           (chord a b epsilon)))))
;-----------------------------------------------------------------

(define perebor (lambda (x h)
  (if (< (* (f x) (f (+ x h))) 0)
      x
      (perebor (+ x h) h))))

;-----------------------------------------------------------------
  
(chord -5 5 0.001)
(display "\n")
(perebor -5 0.001)

;-------------------------------15.2--------------------------------

(display "Введите количество отрезков:")
(define n(read))
(define h (/ 2 n)); вираховується як (b-a)/n

(define (func x)
  (* (/ 1 x) (exp x)))

(define (left_rect)
  (define left_rect_inside (lambda (i res)
    (if (>= i n)
        (* h res)
        (left_rect_inside (+ i 1) (+ res (func (+ 1 (* i h))))))))
  (left_rect_inside 0 0))

(define (right_rect)
  (define right_rect_inside (lambda (i res)
    (if (> i n)
        (* h res)
        (right_rect_inside (+ i 1) (+ res (func (+ 1 (* i h))))))))
  (right_rect_inside 1 0))

(define (middle_rect)
  (define middle_rect_inside (lambda (i res)
    (if (>= i n)
        (* h res)
        (middle_rect_inside (+ i 1) (+ res (func (+ 1 (* h (+ i 0.5)))))))))
  (middle_rect_inside 0 0))

(define (trapezoid)
  (define trapezoid_inside (lambda (i res)
    (if (> i n)
        (* (/ h 2) (+ (* res 2) (+ (func 1) (func (+ 1 (* h n))))))
        (trapezoid_inside (+ i 1) (+ res (func (+ 1 (* h i))))))))
  (trapezoid_inside 1 0))


(display "\n")
(left_rect)
(display "\n")
(right_rect)
(display "\n")
(middle_rect)
(display "\n")
(trapezoid)


  
        
        
  
  








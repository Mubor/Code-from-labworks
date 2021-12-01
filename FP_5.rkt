;............................................(15.1)...........................................................
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (create n d) (cons n d))

(define (print f sign)
  (display (numer f))
  (display sign)
  (display (denom f)))

(define (printeq f1 f2 f3 sign)
  (print f1 sign)
  (display " + ")
  (print f2 sign)
  (display " = ")
  (print f3 sign))

(define (nok a b c)
  (define temp (/ (* a b) (gcd a b)))
    (/ (* temp c) (gcd temp c)))

;створюємо дроби
(define fract1 (create (cons "x" -1)  2))
(define fract2 (create (cons 2 "x")  3))
(define fract3 (create (cons 5 "x")  6))

;запис рівняння
(printeq fract1 fract2 fract3 "/")

(newline)
; спыльний знаменник
(define mn (nok (denom fract1) (denom fract2) (denom fract3)))

;домножуємо

(set! fract1 (create (cons (cons mn (car (numer fract1))) (* mn (cdr (numer fract1)))) (denom fract1)))
(set! fract2 (create (cons (* mn (car (numer fract2))) (cdr (numer fract2))) (denom fract2)))
(set! fract3 (create (cons (* mn (car (numer fract3))) (cdr (numer fract3))) (denom fract3)))



;дылимо на знаменник

(set! fract1 (cons (cons (/ (car ( car (numer fract1))) (denom fract1)) (cdr (car (numer fract1)))) (/ (cdr fract1) (denom fract1))))

fract1

(set! fract1 (cons (- (+ 3 4) 5) "x"))

(newline)
(display fract1)
(display "= 3")

(newline)
(display "x = ")
(display (/ 3 (car fract1)))
;........................................................................(15.2)...................................................................

(newline)
(define (create_list x xmax);створення списку чисел
  (define res `())
  (define (inside i res)
    (if (> i xmax)
        res
        (if (= i 0)
            (inside (+ i 1) res)
            (inside (+ i 1) (append res (list i))))))
  (inside x res))

;-------------------------------------------------------------------------------------------
(define (create_pair_for_one lst ind);створення пар для одного числа
  (define res `())
  (define (inside i res)   
    (if (>= i (length lst))
        res
        (inside (+ i 1) (append res (list(cons (list-ref lst ind) (list-ref lst i)))))))
  (inside 0 res))
;------------------------------------------------------------------------------------------

(define (create_pairs lst);створення пар
  (define res `())
  (define (inside i res)   
    (if (>= i (length lst))
        res
        (inside (+ i 1) (append res (create_pair_for_one lst i)))))
  (inside 0 res))

;-------------------------------------------------------------------------------------------
(define (anum x) (car (list-ref sample x)))
(define (bnum x) (cdr (list-ref sample x)))

(define (r a b) (round(sqrt (+ (* a a) (* b b)))))
(define (fi a b) (atan (/ b a)))

;-----------------------------------------------------------------------------------------------


(define (to_fract p) ; приведення пари з п до дробу
  (string-append (car p) "/" (number->string (cdr p))))

;---------------------------------------------------------------------------------------------

(define (to_pi angle);приведення куту до пари з П
  (define f (create (round angle) 180))
  f
  (define temp (make-rat (numer f) (denom f)))
  (define res (create (string-append (number->string (numer temp)) "П") (denom temp)))
  res)

;--------------------------------------------------------------------------------------------

(define (to_angle radian) (* radian (/ 180 pi)));переведення радіанів у кути
;-----------------------------------------------------------------------------------------------------

(define (make-rat n d);скорочення дробу
    (let ((nod (gcd n d)))
         (cons (/ n nod) (/ d nod))))

;------------------------------------------------------------------------
(define pi 3.1416)
(define nums (create_list -3 3))
(define sample (create_pairs nums))

(define (str r fi);формування запису
  (set! r (number->string r))
  (string-append "z = " r "(cos(" fi ") + i * sin(" fi "))"))


(define (find_sol)
  (define res `())
  (define (inside i)
    (if (>= i (length sample))
        res
        (begin
          (set! res (append res (list(str (r (anum i)(bnum i)) (to_fract(to_pi(to_angle (fi (anum i)(bnum i)))))))))
          (inside (+ i 1)))))
    (inside 0))

(find_sol)



          
        














;.........................................15.1............................................
(#%require (only racket/base random))

(define (minx vect)
  (define ind 0)
  (define (inside i)
    (if (>= i (vector-length vect))
        (vector-ref vect ind)
        (begin
        (if (< (vector-ref vect i) (vector-ref vect ind))
               (set! ind i))
        (inside (+ i 1)))))
  (inside 0))

(define (maxx vect)
  (define ind 0)
  (define (inside i)
    (if (>= i (vector-length vect))
        (vector-ref vect ind)
        (begin
        (if (> (vector-ref vect i) (vector-ref vect ind))
               (set! ind i))
        (inside (+ i 1)))))
  (inside 0))

(define (output vect)
  (define mi (minx vect))
  (define ma (maxx vect))
  (define (inside i)
    (if (< i (vector-length vect))
        (begin
        (if (and(> (vector-ref vect i) mi) (< (vector-ref vect i) ma))
            (begin
              (display (vector-ref vect i))
              (display " ")))
               
        (inside (+ i 1)))))
  (inside 0))

(define (create_vect n)
  (define res `())
  (define (inside i)
    (if (>= i n)
        (list->vector res)
        (begin
          (set! res (append res (list(random 100))))
          (inside (+ i 1)))))
  (inside 0))

(define v (create_vect 10))
v
(output v)
;............................................................15.2.........................................................
(newline)
(define (front q) (car q)) 
(define (rear q)(cdr q))


(define (make-queue)
 (define p (cons `()`() ))
 (cons p p))

(define (null-queue? q)(and(eq? (front q) (rear q)) (eq? (car (front q)) '() )))

(define (push q e)
 (define p (cons e '()))
 (if (null-queue? q)
  (begin (set-car! q p)
   (set-cdr! q p)
  )
  (begin
   (set-cdr! (rear q) p)
   (set-cdr! q p)
  ) ) )



(define (create_queue n nmax)
  (define res (make-queue))
  (define (inside i)
    (if (>= i nmax)
        res
        (begin
          (push res i)
          (inside (+ i 1)))))
  (inside n))

(define (fill_queue lst)
  (define res (make-queue))
  (define (inside i)
    (if (>= i (length lst))
            res
        (begin
          (push res (list-ref lst i))
          (inside (+ i 1)))))
  (inside 0))

(define (merge q1 q2 n)
  (define res `())
  (define temp1 (front q1))
  (define temp2 (front q2))
  (define (inside i q)
    (if (< i n)
        (begin
        (set! res (append res (list(list-ref q i))))
        (inside (+ i 1) q))))
  (inside 0 temp1)
  (inside 0 temp2)
  (fill_queue res))

(define q1 (create_queue 5 10))
(define q2 (create_queue 11 17 ))

q1

q2

(merge q1 q2 5)

               
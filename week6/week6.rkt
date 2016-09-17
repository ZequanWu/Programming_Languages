#lang racket
(provide (all-defined-out))

(define x 3)
(define y (+ x 2)) ; + is a function, call it here

; this is not syntax sugar
(define cube1
  (lambda (x)
    (* x x x)))

; this is syntax sugar
(define (cube3 x)
  (* x x x))

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

; curring function
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define three-to-the (pow2 3))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))
  
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map (cdr xs)))))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))
; works for list contains non-numbers elements, but not work for non-list argument
(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (+ (if (list? (car xs))
                 (sum2 (car xs))
                 0)
              (sum2 (cdr xs))))))
; works for even non-list argument
(define (sum3 xs)
  (if (list? xs)
      (sum2 xs)
      0))
; same as sum2
(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? xs) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [#t (sum4 (cdr xs))]))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

; let:  The expressions are all evaluated in the environment from before the let-expression
(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))

; let*: The expressions are evaluated in the environment produced from the previous bindings
(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

; letrec: The expressions are evaluated in the enviroment that includes all the bindings
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

; Toplevel bindings
;; (define (f x) (+ x (* x b))) ; forward reference okay here, because b is inside procedure
;; (define b 3)
;; (define c (+ b 4)) ; backawrd reference okay
;; ; (define d (+ e 4)) ; not okay (get an error instead of #<undefined, because d is evaluated in order)
;; (define e 5)
; (define f 17) ; not okay: f already defined in this module

; set example
;; (define b 3)
;; (define f (lambda (x) (* 1 (+ x b))))
;; (define c (+ b 4)) ; 7
;; (set! b 5)
;; (define z (f 4)) ; 9
;; (define w c) ; 7

;; don't do this
; A general principle: If something you need not to change
; might change, make a local copy of it.
;; (define b 3)
;; (define f
;;   (let ([b b])
;;     (lambda (x) (* 1 (+ x b)))))

(define mpr (mcons 1 (mcons #t "hi")))
(mcar mpr) ; get the first element in mcons cell
(mcdr mpr) ; get the rest mcons cell
(set-mcdr! mpr 45) ; change the rest mcons cell to 45
(set-mcar! mpr 12) ; change the first element in mcons cell

; racket evaluates all arguments before function call
(define (factorial-normal x)
  (if (= x 0)
      1 (* x (factorial-normal (- x 1)))))
(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))
; no matter what the value of x is, factorial-bad will never terminate. The reason is that the 3 arugments in my-if-bad function will evaluate before it is called.
(define (factorial-bad x)
  (my-if-bad (= x 0)
             1
             (* x
                (factorial-bad (- x 1)))))

; e2 and e3 should be zero-argument functions (delays evaluation
(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3))) ; e2 and e3 are functions take no arugmebnts
(define (factorial-okay x)
  (my-if-strange-but-works
   (= x 0)
   (lambda () 1)
   (lambda () (* (factorial-okay (- x 1))))))
; A zero-argument function used to delay evalution is called a thunk
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [else (+ (y-thunk) (my-mult (- x 1) y-thunk))]))
   
; delay and force
(define (my-delay th)
  (mcons #f th))
(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

; using streams
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
             (let ([pr (stream)])
               (if (tester (car pr))
                   ans
                   (f (cdr pr) (+ 1 ans)))))])
    (f stream 1)))
; defining streams. Using pairs and thunks
; streams form: '(next-answer . next-thunk)
; stream contains infinit 1
(define ones (lambda () (cons 1 ones)))

; stream contains: 1 2 3 4 5 ...
;; (define (f x) (cons x (lambda () (f (+ x 1)))))
;; (define nats (lambda () (f 1)))
;; (define nats       
;;   (letrec ([f (lambda (x)
;;              (cons x (lambda () (f (+ x 1)))))])
;;     (lambda () (f 1))))
;; (define powers-of-two
;;   (letrec ([f (lambda (x)
;;              (cons x (lambda () (f (* x 2)))))])
;;     (lambda () (f 2))))
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
             (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
(define nats (stream-maker + 1))
(define powers-of-two (stream-maker * 2))

; inefficient function
(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))
; efficient function
;; (define (fibonacci2 x)
;;   (letrec ([f (lambda (acc1 acc2 y))
; efficient way to find fib numbers by storing visited values in table
(define fibonacci3
  (letrec ([memo null] ; memo will be changed later. It's a list of pair
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))

; add 1 to the list a, every time fun is called.
(define fun
  (letrec ([a null]
           [f (lambda ()
                (begin (set! a (cons 1 a))
                       a))])
    f))

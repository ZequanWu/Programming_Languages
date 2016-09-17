#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; problem 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;; problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([rest-stream (cdr (s))])
        (cons (car (s)) (stream-for-n-steps rest-stream (- n 1))))))

;; problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0) (- x) x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; problem 6
(define dan-then-dog
  (letrec ([f (lambda (x y)
                (cons x (lambda () (f y x))))])
    (lambda () (f "dan.jpg" "dog.jpg"))))
(define dan-then-dog2
  (letrec ([dan-st (lambda () (cons "dan.jpg" dog-st))]
           [dog-st (lambda () (cons "dog.jpg" dan-st))])
    dan-st))
(define dan-then-dog3
  (cons "dan.jpg"
        (lambda () (cons "dog.jpg" dan-then-dog3))))

;; problem 7
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (stream-add-zero (cdr (s))))))

;; problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([nth-v (vector-ref vec n)])
                      (if (and (pair? nth-v) (equal? (car nth-v) v))
                          nth-v
                          (f (+ n 1))))))])
           (f 0)))

;; problem 10
(define (cached-assoc xs n)
  (let ([cashe (make-vector n #f)]
        [next 0])
    (lambda (v)
      (or (vector-assoc v cashe)
          (let ([new-ans (assoc v xs)])
            (and new-ans
                 (begin (vector-set! cashe next new-ans)
                        (set! next (if (= next (- n 1))
                                       0
                                       (+ next 1)))
                        new-ans)))))))

;; problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([ex1 e1]
              [f (lambda ()
                       (if (<= ex1 e2)
                           #t
                           (f)))])
         (f))]))

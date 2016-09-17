#lang racket
(provide (all-defined-out))

;; a cosmetic macro -- adds then, else
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))
; (my-if foo then bar else baz) -> (if foo bar baz)

;; a macro to replace an expression with another one
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))
; (comment-out (car null) (+ 3 4)) -> (+ 3 4)

;; (define (my-delay th)
;;   (mcons #f th))
;; (define (my-force p)
;;   (if (mcar p)
;;       (mcdr p)
;;       (begin (set-mcar! p #t)
;;              (set-mcdr! p ((mcdr p)))
;;              (mcdr p))))

; makes it so users do *not* write the thunk when using my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))
; this is really bad because it evaluates e multiple times
(define-syntax my-force-macro1
  (syntax-rules ()
    [(my-for e)
     (if (mcar e)
         (mcdr e)
         (begin (set-mcar! e #t)
                (set-mcdr! e ((mcdr e)))
                (mcdr e)))]))

; A hygienic macro system:
; 1. Secretly renames local variables in macros with fresh names
; 2. Looks up variables used in macros where the macro is defined
(define-syntax dbl
  (syntax-rules ()
    [(dbl x) (let ([y 1])
               (* 2 x y))]))
; example: (let ([y 7]) (dbl y)) -> get 14 not 2
(define-syntax dbl2
  (syntax-rules ()
    [(dbl2 x) (* 2 x)]))
; example: (let ([* +]) (dbl2 42)) -> get 84 not 44

; a loop that executes body hi - lo times
; notice use of local variables
(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           [h hi])
       (letrec ([loop (lambda (it)
                        (if (> it h)
                            #t
                            (begin body (loop (+ 1 it)))))])
         (loop l)))]))

(define-syntax let2
  (syntax-rules ()
    [(let2 () body) body]
    [(let2 (var val) body)
     (let ([var val]) body)]
    [(let2 (var1 val1 var2 val2) body)
     (let ([var1 val1])
       (let ([var2 val2])
         body))]))

; the special ... lets us take any number of arguments
; Note: nothing prevents infinite code generation except
; the macro definer being careful
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body) body]
    [(my-let* (var1 val1) body)
     (let ([var1 val1]) body)]
    [(my-let* (var1 val1
               var-rest val-rest ...)
              body)
     (let ([var1 val1])
       (my-let* (var-rest val-rest ...)
                body))]))


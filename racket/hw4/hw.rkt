
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
	(if (> low high)
		null
		(cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) 
	(map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
	(if (< n 0)
		(error "list-nth-mod: negative number")
		(if (null? xs)
			(error "list-nth-mod: empty list")
			(car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
	(if (> n 0)
		(cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
		null))

(define funny-number-stream 
	(letrec ([f 
		(lambda (x) (cons 
			(if (= 0 (remainder x 5)) (- 0 x) x) 
			(lambda () (f (+ x 1)))))])
	(lambda () (f 1))))

(define dan-then-dog 
	(lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

(define (stream-add-zero s) 
	(lambda () (cons (cons 0 (car (s))) 
					(stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys) 
	(define (f n) 
		(lambda () (cons 
			(cons (list-nth-mod xs n) (list-nth-mod ys n))
			(f (+ 1 n)))))
	(f 0))

(define (vector-assoc v vec)
	(letrec ([l (vector-length vec)]
		[f (lambda (n) 
			(if (= l n) #f
				(let ([ref (vector-ref vec n)])
					(if (pair? ref) 
						(if (equal? v (car ref)) ref (f (+ 1 n)))
						(f (+ 1 n))))))])
	(f 0)))

(define (cached-assoc xs n)
	(let ([cache (make-vector n #f)]
		[pos 0])
	(lambda (v)
		(let ([cr (vector-assoc v cache)])
			(if cr cr
			(let ([xr (assoc v xs)])
				(if xr 
					(begin (vector-set! cache pos xr)
						(set! pos (if (= (+ 1 pos) n) 0 (+ 1 pos)))
						xr)
					#f)))))))

(define-syntax while-less
	(syntax-rules (do)
		[(while-less e1 do e2) 
		(letrec ([x e1]
			[thunk (lambda () (cons e2 thunk))]
			[f (lambda (t) 
				(if (< (car (t)) x)
					(f (cdr (t)))
					#t))])
		(f thunk))]))
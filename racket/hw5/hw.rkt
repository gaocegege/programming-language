;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist rlist)
    (if (null? rlist)
        (aunit)
        (apair (car rlist) (racketlist->mupllist (cdr rlist)))))
(define (mupllist->racketlist mlist)
    (if (isaunit? mlist)
        null
        (cons (fst mlist) (mupllist->racketlist (snd mlist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2)) 
               (if (> (int-num v1) (int-num v2))
                    (eval-under-env (ifgreater-e3 e) env)
                    (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL addition applied to non-number")))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
            (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
         (if (closure? v1)
             (let* ([nameopt (fun-nameopt (closure-fun v1))]
                    [formal (fun-formal (closure-fun v1))]
                    [body (fun-body (closure-fun v1))]
                    [newenv (cons (cons formal v2) (closure-env v1))])
                (if nameopt
                    (eval-under-env body (cons (cons nameopt v1) newenv ))
                    (eval-under-env body newenv )))
             (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
            (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
            (if (apair? v)
                (apair-e1 v)
                (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
            (if (aunit? v)
                (int 1)
                (int 0)))]
        [(closure? e)
         e]
        [(aunit? e)
         e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
    (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) 
    (if (null? lstlst)
        e2
        (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
    (mlet* (list (cons "_x" e1) (cons "_y" e2))
        (ifgreater (var "_x") (var "_y") e4 
            (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map (fun #f "fun"
    (fun "helper" "mlist"
        (ifaunit (var "mlist") (var "mlist")
            (apair (call (var "fun") (fst (var "mlist"))) (call (var "helper") (snd (var "mlist"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
            (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars-helper e shadow)
  (cond [(var? e) 
         (if (set-member? shadow (var-string e))
            (cons e (set))
            (cons e (set (var-string e))))]
        [(add? e) 
         (let ([v1 (compute-free-vars-helper (add-e1 e) shadow)]
               [v2 (compute-free-vars-helper (add-e2 e) shadow)])
           (cons (add (car v1) (car v2))
                (set-union (cdr v1) (cdr v2))))]
        ;; CHANGE add more cases here
        [(int? e)
         (cons e (set))]
        [(ifgreater? e)
         (let ([v1 (compute-free-vars-helper (ifgreater-e1 e) shadow)]
               [v2 (compute-free-vars-helper (ifgreater-e2 e) shadow)]
               [v3 (compute-free-vars-helper (ifgreater-e3 e) shadow)]
               [v4 (compute-free-vars-helper (ifgreater-e4 e) shadow)])
           (cons (ifgreater (car v1) (car v2) (car v3) (car v4))
                (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4))))]
        [(fun? e)
         (let ([v (compute-free-vars-helper (fun-body e) shadow)])
            (cons (fun-challenge (fun-nameopt e) (fun-formal e) (car v) (cdr v))
                (cdr v)))]
        [(mlet? e)
         (let ([v1 (compute-free-vars-helper (mlet-e e) shadow)]
               [v2 (compute-free-vars-helper (mlet-body e) (set-add shadow (mlet-var e)))])
            (cons (mlet (mlet-var e) (car v1) (car v2))
                (set-union (cdr v1) (cdr v2))))]
        [(call? e)
         (let ([v1 (compute-free-vars-helper (call-funexp e) shadow)]
               [v2 (compute-free-vars-helper (call-actual e) shadow)])
            (cons (call (car v1) (car v2))
                (set-union (cdr v1) (cdr v2))))]
        [(apair? e)
         (let ([v1 (compute-free-vars-helper (apair-e1 e) shadow)]
               [v2 (compute-free-vars-helper (apair-e2 e) shadow)])
            (cons (apair (car v1) (car v2))
                (set-union (cdr v1) (cdr v2))))]
        [(fst? e)
         (let ([v (compute-free-vars-helper (fst-e e) shadow)])
            (cons (fst (car v))
                (cdr v)))]
        [(snd? e)
         (let ([v (compute-free-vars-helper (snd-e e) shadow)])
            (cons (snd (car v))
                (cdr v)))]
        [(isaunit? e)
         (let ([v (compute-free-vars-helper (isaunit-e e) shadow)])
            (cons (isaunit (car v))
                (cdr v)))]
        [(closure? e)
         (let ([v (compute-free-vars-helper (closure-fun e) shadow)])
            (cons (closure (closure-env e) (car v))
                (cdr v)))]
        [(aunit? e)
         (cons e (set))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
(define (compute-free-vars e) 
    (car (compute-free-vars-helper e (set))))

(define (env-in-free env vars)
    (letrec ([h (lambda (env)
                (if (null? env)
                    null
                    (if (set-member? vars (car (car env)))
                        (cons (car env) (h (cdr env)))
                        (h (cdr env)))))])
        (h env)))
;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
    (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2)) 
               (if (> (int-num v1) (int-num v2))
                    (eval-under-env-c (ifgreater-e3 e) env)
                    (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL addition applied to non-number")))]
        [(fun-challenge? e)
         (closure env e)]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)])
            (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([v1 (eval-under-env-c (call-funexp e) env)]
               [v2 (eval-under-env-c (call-actual e) env)])
         (if (closure? v1)
             (let* ([nameopt (fun-challenge-nameopt (closure-fun v1))]
                    [formal (fun-challenge-formal (closure-fun v1))]
                    [body (fun-challenge-body (closure-fun v1))]
                    [freevars (fun-challenge-freevars (closure-fun v1))]
                    [newenv (cons (cons formal v2) (closure-env v1))])
                (if nameopt
                    (eval-under-env-c body (env-in-free (cons (cons nameopt v1) newenv) freevars))
                    (eval-under-env-c body (env-in-free newenv freevars))))
             (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
            (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
            (if (apair? v)
                (apair-e1 v)
                (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
            (if (aunit? v)
                (int 1)
                (int 0)))]
        [(closure? e)
         e]
        [(aunit? e)
         e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
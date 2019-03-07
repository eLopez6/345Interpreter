#lang racket
(require "simpleParser.rkt")

(define emptystate (lambda () '(()())))
(define startcatch '())
(define startfinally '())
(define startcontinue '())
(define startbreak '())



;;where it starts
(define main
  (lambda (filename)
    (call/cc
      (lambda (k)
        (mstate (parser filename)
                (list (emptystate))
                startcatch
                startfinally
                startcontinue
                startbreak
                (lambda (val) (k (if (number? val) val (if val 'true 'false)))))))))


(define mstate
  (lambda (lis state catch finally continue break return)
    (cond
      [(null? lis) state]
      [(eq? (caar lis) 'return)
       (return (evaluate (cadar lis) state))]
      [(eq? (caar lis) 'var)
       (mstate (cdr lis) (instantiatevar (cdar lis) state) catch finally continue break return)]
      [(eq? (caar lis) '=)
       (mstate (cdr lis) (updatevar (cdar lis) state) catch finally continue break return)]
      [(eq? (caar lis) 'begin)
       (mstate (cdr lis) (removestatelayer (mstate (cdar lis) (addstatelayer state) catch finally continue break return)) catch finally continue break return)]
      [(eq? (caar lis) 'if)
       (mstate (cdr lis) (mif (car lis) state catch finally continue break return) catch finally continue break return)]
      [(eq? (caar lis) 'while)
       (mstate (cdr lis) (truncstate (call/cc (lambda (break) (mwhile (car lis) state catch finally continue break return))) (len state)) catch finally continue break return)]
      [(and (eq? (caar lis) 'break) (not (null? break)))
       (break state)]
      [(eq? (caar lis) 'break)
       (error "Break Outside of Loop")]
      [(and (eq? (caar lis) 'continue) (not (null? continue)))
       (continue state)]
      [(eq? (caar lis) 'continue)
       (error "Continue Outside of Loop")]
      [(and (eq? (caar lis) 'throw) (not (null? throw)))
       (catch (evaluate (cadar lis) state))]
      [(eq? (caar lis) 'throw)
       (error "Throw Outside of try")])))

      


;adds a state to the list of states
(define addstatelayer
  (lambda (state)
    (cons (emptystate) state)))

;removes a layer from the list of states
(define removestatelayer
  (lambda (state)
    (cdr state)))


(define truncstate
  (lambda (state length)
    (cond
      [(eq? (len state) length) state]
      [else (truncstate (removestatelayer state) length)])))

(define mtcf
  (lambda (trylis catchlis finallylis state catch finally continue break return)
       (mstate finallylis
               (call/cc (lambda (finally)
                  (mstate catchlis
                          (call/cc (lambda (catch)
                             (mstate trylis state catch finally continue break return)))
                          catch finally continue break return)))
               catch finally continue break return)))
       
;;(define mcatch
  ;(lambda (val state catch finally continue brea


(define evaluate
  (lambda (lis state)
    (cond
      [(or (boolean? lis) (number? lis))              lis]
      [(not (list? lis))                              (lookup-all lis state)]
      [(isincluded (car lis) '(+ - * / %))            (mvalue lis state)]
      [(isincluded (car lis) '(> >= < <= == || && !)) (mbool lis state)])))


(define operator car)
(define operand1 cadr)
(define operand2 caddr)

;; mvalue for math
(define mvalue
  (lambda (lis state)
    (cond
      [(number? lis)
       lis]
      [(not (list? lis))
       (lookup-all lis state)]
      [(eq? (operator lis) '+)
       (+ (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(and (eq? (operator lis) '-) (eq? (len lis) 3))
       (- (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))] ; subtractions
      [(eq? (operator lis) '-)
       (- (mvalue (operand1 lis) state))] ; negation
      [(eq? (operator lis) '*)
       (* (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '/)
       (quotient (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '%)
       (remainder (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))])))


;;mbool for boolean logic
(define mbool
  (lambda (lis state)
    (cond
      [(boolean? lis)           lis]
      [(not (list? lis))        (lookup-all lis state)]
      [(eq? (operator lis) '>)  (> (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '>=) (>= (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '<)  (< (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '<=) (<= (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '==) (eq? (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '!=) (not (eq? (mvalue (operand1 lis) state) (mvalue (operand2 lis) state)))]
      [(eq? (operator lis) '&&) (and (mbool (operand1 lis) state) (mbool (operand2 lis) state))]
      [(eq? (operator lis) '||) (or (mbool (operand1 lis) state) (mbool (operand2 lis) state))]
      [(eq? (operator lis) '!)  (not (mbool (operand1 lis) state))])))



;; for if statements
(define mif
  (lambda (lis state catch finally continue break return)
    (cond
      [(mbool (cadr lis) state) (mstate (list (caddr lis)) state catch finally continue break return)]
      [(hasNestedIf lis)        (mif (cadddr lis) break state)]
      [else                     (mstate (cdddr lis) state catch finally continue break return)])))


;; checks if there's and else if
(define hasNestedIf
  (lambda (lis)
    (and (>= (len lis) 4) (eq? (car (cadddr lis)) 'if))))


;; calc length using accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
        acc
        (len-acc (cdr lis) (+ 1 acc)))))

(define len
  (lambda (lis)
    (len-acc lis 0)))



;;implementation of while loops      
(define mwhile
  (lambda (lis state catch finally continue break return)
       (cond
         [(mbool (cadr lis) state) (mwhile lis (truncstate (call/cc (lambda (continue) (mstate (cddr lis) state catch finally continue break return))) (len state)) catch finally continue break return)]
         [else                     state])))

;; adds a value into the most recent state
(define addtostate
  (lambda (val state)
    (cons
          (cons (cons val (caar state))
                (list (cons '() (cadar state))))
          (cdr state))))

    
;; Instatiate variables
(define instantiatevar
  (lambda (lis state)
    (cond
      [(checkexists (car lis) (car state))
       (error (car lis) "Redefining a variable")]
      [(null? (cdr lis))
       (addtostate (car lis) state)]
      [else
       (updatevar lis (addtostate (car lis) state))])))


;; Check if a variable already has been declared, returns true if in current state (expects single layer state)
(define checkexists
  (lambda (var state)
    (cond
      [(null? (car state))    #f]
      [(eq? (caar state) var) #t]
      [else                   (checkexists var (cons (cdar state) (list (cdadr state))))])))


;; default use of updatevar
(define updatevar
  (lambda (lis state)
    (updatevar-all-acc lis state state '())))


;; set variable in most recent state for list of states
(define updatevar-all-acc
  (lambda (lis state all-state acc)
    (cond
      [(null? state)
       (error (car lis) "Used Before Declared")]
      [(checkexists (car lis) (car state))
       (append acc (cons (updatevar-acc (car lis) (evaluate (cadr lis) all-state) (car state) (emptystate)) (cdr state)))]
      [else
       (updatevar-all-acc lis (cdr state) all-state (append acc (list (car state))))])))


;; set variable for single state
(define updatevar-acc
  (lambda (var val state acc)
    (cond
      [(null? (car state)) acc]
      [(eq? (caar state) var)
        (updatevar-acc var
                       val
                       (cons (cdar state) (list (cdadr state)))
                       (cons (cons (caar state) (car acc)) (list (cons val (cadr acc)))))]
      [else
       (updatevar-acc var
                      val
                      (cons (cdar state) (list (cdadr state)))
                      (cons (cons (caar state) (car acc)) (list (cons (caadr state) (cadr acc)))))])))


;;finds saved value of var
(define lookup
  (lambda (var state break)
    (cond
      [(eq? var 'true)                                    (break #t)]
      [(eq? var 'false)                                   (break #f)] 
      [(null? (car state))                                '()]
      [(and (eq? (caar state) var) (null? (caadr state))) (error var "Use Before Assigning")]
      [(eq? (caar state) var)                             (break (caadr state))]
      [else                                               (lookup var
                                                                  (cons (cdar state) (list (cdadr state)))
                                                                  break)])))

; if lookup finds something, a break is called
; otherwise, no variable var exists in state
(define lookup-all
  (lambda (var state)
    (call/cc
     (lambda (k)
       (cond
         [(list? (map (lambda (xss) (lookup var xss k)) state))
          (error var "Used Before Declared")])))))

      
;;checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis)       #f]
      [(eq? (car lis) a) #t]
      [else              (isincluded a (cdr lis))])))







(define test
  (lambda (val file)
    (cond
      [(not (eq? val (main file))) (error "TEST FAILED" file)])))

(require racket/exn)
(define testError
  (lambda (err file)
    (cond
      [(with-handlers ([exn:fail? (lambda (exn) (not (string-contains? (exn->string exn) err)))]) (main file)) (error "TEST FAILED" file)])))


(test 150 "../testfiles/1.txt")
(test -4 "../testfiles/2.txt")
(test 10 "../testfiles/3.txt")
(test 16 "../testfiles/4.txt")
(test 220 "../testfiles/5.txt")
(test 5 "../testfiles/6.txt")
(test 6 "../testfiles/7.txt")
(test 10 "../testfiles/8.txt")
(test 5 "../testfiles/9.txt")
(test -39 "../testfiles/10.txt")
(testError "y: Used Before Declared" "../testfiles/11.txt")
(testError "x: Used Before Declared" "../testfiles/12.txt")
(testError "x: Use Before Assigning" "../testfiles/13.txt")
(testError "x: Redefining a variable" "../testfiles/14.txt")
(test 'true "../testfiles/15.txt")
(test 100 "../testfiles/16.txt")
(test 'false "../testfiles/17.txt")
(test 'true "../testfiles/18.txt")
(test 128 "../testfiles/19.txt")
(test 12 "../testfiles/20.txt")


(test 20 "../testfiles/2-1.txt")
(test 164 "../testfiles/2-2.txt")
(test 32 "../testfiles/2-3.txt")
(test 2 "../testfiles/2-4.txt")
(testError "min: Used Before Declared" "../testfiles/2-5.txt")
(test 25 "../testfiles/2-6.txt")
(test 21 "../testfiles/2-7.txt")
(test 6 "../testfiles/2-8.txt")
(test -1 "../testfiles/2-9.txt")
(test 789 "../testfiles/2-10.txt")
(testError "y: Used Before Declared" "../testfiles/2-11.txt")
(testError "a: Used Before Declared" "../testfiles/2-12.txt")
(testError "Break Outside of Loop" "../testfiles/2-13.txt")
(test 12 "../testfiles/2-14.txt")
(test 125 "../testfiles/2-15.txt")
(test 110 "../testfiles/2-16.txt")
(test 2000400 "../testfiles/2-17.txt")
(test 101 "../testfiles/2-18.txt")
(test 789 "../testfiles/2-19.txt")











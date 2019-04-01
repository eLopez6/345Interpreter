#lang racket
;Mark Goldberg
;Emilio Lopez
;Jacob Alspaw

;(require "simpleParser.rkt")
(require "functionParser.rkt")

;; Definitions for abstraction
(define emptyenviro (lambda () '(()())))
(define emptystate (lambda () '(()())))
(define startcatch '())
(define startfinally '())
(define startcontinue '())
(define startbreak '())

;;; Main program

;; Main function
(define main
  (lambda (filename)
    (checkbool
     (call/cc
      (lambda (k)
        (menvironment (parser filename) (list (emptyenviro)) k))))))

(define checkbool
  (lambda (val)
    (cond
      [(eq? val #t)   'true]
      [(eq? val #f)   'false]
      [else           val])))


(define menvironment
  (lambda (lis enviro mainreturn)
    (cond
      [(null? lis) (mainreturn (car (runfunction 'main '() enviro)))]
      [(eq? (caar lis) 'function)  (menvironment (cdr lis) (createfunction (car lis) enviro) mainreturn)]
      [else
       (menvironment (cdr lis)
                     (mstate (list (car lis))
                                   enviro
                                   '()
                                   '()
                                   '()
                                   '()
                                   '())
                     mainreturn)])))


(define functionname
  (lambda (lis)
    (cadr lis)))

(define functionparam
  (lambda (lis)
    (caddr lis)))

(define functionbody
  (lambda (lis)
    (cadddr lis)))

(define createfunction
  (lambda (lis enviro)
    (cond
      [(checkexists (functionname lis)  (car enviro))
       (error (car lis) "Redefining a function")]
      [else
       (cons (cons (cons (functionname lis) (caar enviro))
                   (list (cons
                          (list (functionparam lis)
                                (functionbody lis)
                                (lambda (params currentenviro)
                                  (list (functionparam lis) (getparamval params currentenviro))))
                          (cadar enviro))))
                   (cdr enviro))])))


(define runfunction
  (lambda (funcname paramlis enviro)
    (call/cc
     (lambda (startreturn)
       (mstate (cadr (lookup-all funcname enviro))
               (cons ((caddr (lookup-all funcname enviro)) paramlis enviro) enviro)
               startcatch
               startfinally
               startcontinue
               startbreak
               startreturn)))))


(define getparamval
  (lambda (lis enviro)
    (cond
      [(null? lis) '()]
      [else (cons (evaluate (car lis) enviro) (getparamval (cdr lis) enviro))])))



;; Mstate
(define mstate
  (lambda (lis state catch finally continue break return)
    (cond
      [(null? lis) state]
      
      [(number? (car lis))
       (mstate (cdr lis) state catch finally continue break return)]
      
      [(boolean? (car lis))
       (mstate (cdr lis) state catch finally continue break return)]
      
      [(eq? (caar lis) 'return)
       (return (cons (evaluate (cadar lis) state) state))]
      
      [(eq? (caar lis) 'var)
       (mstate (cdr lis)
               (instantiatevar (cdar lis) state)
               catch finally continue break return)]
      
      [(eq? (caar lis) '=)
       (mstate (cdr lis)
               (updatevar (cdar lis) state)
               catch finally continue break return)]
      
      [(eq? (caar lis) 'begin)
       (mstate (cdr lis)
               (removestatelayer (mstate (cdar lis) (addstatelayer state)
                                         catch finally continue break return))
               catch finally continue break return)]
      
      [(eq? (caar lis) 'if)
       (mstate (cdr lis)
               (mif (car lis) state
                    catch finally continue break return)
               catch finally continue break return)]

      [(eq? (caar lis) 'while)
       (mstate (cdr lis)
               (truncstate
                (call/cc (lambda (break)
                           (mwhile (car lis) state
                                   catch finally continue break return)))
                (len state))
               catch finally continue break return)]
      
      [(and (eq? (caar lis) 'break) (not (null? break)))
       (break state)]
      
      [(eq? (caar lis) 'break)
       (error "Break Outside of Loop")]
      
      [(and (eq? (caar lis) 'continue) (not (null? continue)))
       (continue state)]
      
      [(eq? (caar lis) 'continue)
       (error "Continue Outside of Loop")]

      [(eq? (caar lis) 'try)
       (mstate (cdr lis)
               (mtcf (cadar lis)
                     (caddar lis)
                     (car (cdddar lis))
                     state catch finally continue break return)
               catch finally continue break return)] 
      
      [(and (eq? (caar lis) 'throw)
            (not (null? catch)))
       (catch (cons (list (evaluate (cadar lis) state)) state))]
      
      [(eq? (caar lis) 'throw)
       (error "Throw Outside of try")]
      
      [(eq? (caar lis) 'funcall)
       (something (mstate (cadr (lookup-all (cadar lis) (truncstate state 1)))
                          (cons ((caddr (lookup-all (cadar lis) (truncstate state 1))) (getparamval (cddr lis) state) (truncstate state 1)) (truncstate state 1))
                          startcatch
                          startfinally
                          startcontinue
                          startbreak
                          '()))])))


       ;(mstate (cons (evaluate (car lis) state) (cdr lis)) (truncstate (cdr (mstate (cadr (lookup-all (cadar lis) (truncstate state 1)))
        ;                                                           (cons ((caddr (lookup-all (cadar lis) (truncstate state 1))) (getparamval (cddr lis) state) (truncstate state 1)) (truncstate state 1))
         ;                                                          startcatch
          ;                                                         startfinally
           ;                                                        startcontinue
            ;                                                       startbreak
             ;                                                      '())) 1)
              ; catch finally continue break return)])))
;(list (mstate (car lis) state catch finally continue break return))

(define something
  (lambda (returnval lis catch finally continue break return)
    ;returnval))
    (mstate (cons (car returnval) (cdr lis)) (truncstate (cdr returnval) 1) catch finally continue break return))) 
    
    


;; Adds a state to the list of states
(define addstatelayer
  (lambda (state)
    (cons (emptystate) state)))


;; Removes a layer from the list of states
(define removestatelayer
  (lambda (state)
    (cdr state)))


;; Removes state frames until it's size length
(define truncstate
  (lambda (state length)
    (cond
      [(eq? (len state) length) state]
      [else                     (truncstate (removestatelayer state) length)])))
  

;; Mtry-catch-finally
(define mtcf
  (lambda (trylis catchlis finallylis state catch finally continue break return)
    (evalfinally finallylis
                 (call/cc
                  (lambda (finally)
                    (evalcatch catchlis
                               (call/cc
                                (lambda (catch)
                                  (evaltry trylis
                                           state catch finally continue break return))) ; conts for evaltry
                               catch finally continue break return))) ; conts for evalcatch
                 catch finally continue break return))) ; conts for evalfinally


;; Evaluate the full try block
(define evaltry
  (lambda (trylis state catch finally continue break return)
    (if (null? trylis)
        state
        (finally (removestatelayer (mstate trylis
                                           (addstatelayer state)
                                           catch finally continue break return))))))


;; Evaluate the entirety of the block
(define evalcatch
  (lambda (catchlis state catch finally continue break return)
    (if (null? catchlis)
        state
        (finally (removestatelayer (mstate (caddr catchlis)
                                           (instantiatevar
                                            (append (cadr catchlis) (car state))
                                            (addstatelayer (cdr state)))
                                           catch finally continue break return))))))


;; Evaluate the entirety of the finally block
(define evalfinally
  (lambda (finallylis state catch finally continue break return)
    (if (null? finallylis)
        state
        (removestatelayer (mstate (cadr finallylis)
                                  (addstatelayer state)
                                  catch finally continue break return)))))

; abstractions for operators and operands
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

;; Evaluates the expression
(define evaluate
  (lambda (lis state)
    (cond
      [(or (boolean? lis) (number? lis))                   lis]
      [(not (list? lis))                                   (lookup-all lis state)]
      [(isincluded (operator lis) '(+ - * / %))            (mvalue lis state)]
      [(isincluded (operator lis) '(> >= < <= == || && !)) (mbool lis state)]
      [(eq? (operator lis) 'funcall)                       (car (runfunction (cadr lis) (getparamval (cddr lis) state) (truncstate state 1)))])))


;; Mvalue for numeric operations
(define mvalue
  (lambda (lis state)
    ; f makes the code more readable, since the behavior is almost always (operator (operands)
    ; f applies the input operator the mvalue of the expressions (same as in mvalue above)
    (let [(f (lambda (op)
               (op (evaluate (operand1 lis) state)
                   (evaluate (operand2 lis) state))))]
      (cond
        [(number? lis)
                                      lis]
        [(not (list? lis))
                                      (lookup-all lis state)]
        [(eq? (operator lis) '+)      (f +)]
        [(and (eq? (operator lis) '-)
              (eq? (len lis) 3))
                                      (f -)]
        [(eq? (operator lis) '-)      (- (mvalue (operand1 lis) state))] ; negation
        [(eq? (operator lis) '*)      (f *)]
        [(eq? (operator lis) '/)      (f quotient)]
        [(eq? (operator lis) '%)      (f remainder)]))))


;; Mbool for boolean logic
(define mbool
  (lambda (lis state)
    ; f and b just make the code more readable
    ; f applies the input operator the mvalue of the expressions (same as in mvalue above)
    ; b applies the input comparison to the mbool of the expression
    (let [(f (lambda (op)
              (op (evaluate (operand1 lis) state)
                  (evaluate (operand2 lis) state))))
          (b (lambda (op)
               (op (evaluate (operand1 lis) state)
                   (evaluate (operand2 lis) state))))]
      (cond
        [(boolean? lis)           lis]
        [(not (list? lis))        (lookup-all lis state)]
        [(eq? (operator lis) '>)  (f >)]
        [(eq? (operator lis) '>=) (f >=)]
        [(eq? (operator lis) '<)  (f <)]
        [(eq? (operator lis) '<=) (f <=)]
        [(eq? (operator lis) '==) (f (lambda (x y) (eq? x y)))]
        [(eq? (operator lis) '!=) (f (lambda (x y) (not (eq? x y))))]
        [(eq? (operator lis) '&&) (b (lambda (x y) (and x y)))] ; since and is a macro, need to wrap in lambda
        [(eq? (operator lis) '||) (b (lambda (x y) (or x y)))] ; since or is a macro, need to wrap in lambda
        [(eq? (operator lis) '!)  (not (mbool (operand1 lis) state))]))))


;; for if statements
(define mif
  (lambda (lis state catch finally continue break return)
    (cond
      [(mbool (cadr lis) state) (mstate (list (caddr lis))
                                        state catch finally continue break return)]
      [(hasNestedIf lis)        (mif (cadddr lis)
                                     state catch finally continue break return)]
      [else                     (mstate (cdddr lis)
                                        state catch finally continue break return)])))


;; Checks if there's and else if
(define hasNestedIf
  (lambda (lis)
    (and (>= (len lis) 4)
         (eq? (car (cadddr lis)) 'if))))


;; Calculates length using accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
        acc
        (len-acc (cdr lis) (+ 1 acc)))))


;; Calculates the length of a list
(define len
  (lambda (lis)
    (len-acc lis 0)))


;; Mwhile    
(define mwhile
  (lambda (lis state catch finally continue break return)
    (cond
      [(mbool (cadr lis) state)
       (mwhile lis
               (truncstate
                (call/cc
                 (lambda (continue)
                   (mstate (cddr lis)
                           state catch finally continue break return)))
                (len state))
               catch finally continue break return)]
      [else
       state])))


;; Adds a value into the most recent state
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
      [else                   (checkexists var
                                           (cons (cdar state)
                                                 (list (cdadr state))))])))


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
       (append acc (cons (updatevar-acc (car lis)
                                        (evaluate (cadr lis) all-state)
                                        (car state)
                                        (emptystate))
                         (cdr state)))]
      [else
       (updatevar-all-acc lis
                          (cdr state)
                          all-state
                          (append acc
                                  (list (car state))))])))


;; Set variable for single state
(define updatevar-acc
  (lambda (var val state acc)
    (cond
      [(null? (car state)) acc]
      [(eq? (caar state) var)
        (updatevar-acc var
                       val
                       (cons (cdar state)
                             (list (cdadr state)))
                       (cons (cons (caar state)
                                   (car acc))
                             (list (cons val
                                         (cadr acc)))))]
      [else
       (updatevar-acc var
                      val
                      (cons (cdar state)
                            (list (cdadr state)))
                      (cons (cons (caar state)
                                  (car acc))
                            (list (cons (caadr state)
                                        (cadr acc)))))])))


;; Finds saved value of var within a state
(define lookup
  (lambda (var state break)
    (cond
      [(eq? var 'true)             (break #t)]
      [(eq? var 'false)            (break #f)] 
      [(null? (car state))         '()]
      [(and (eq? (caar state) var)
            (null? (caadr state))) (error var "Use Before Assigning")]
      [(eq? (caar state) var)      (break (caadr state))] ; return the var's value
      [else                        (lookup var
                                           (cons (cdar state)
                                                 (list (cdadr state)))
                                           break)])))


;; If lookup finds the var, a break is called
;; Otherwise, no variable var exists in state and an error is raised.
(define lookup-all
  (lambda (var state)
    (call/cc
     (lambda (k)
       (cond
         [(list? (map (lambda (xss) (lookup var xss k)) state))
          state])))))
          ;(error var "Used Before Declared")])))))

      
;; Checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis)       #f]
      [(eq? (car lis) a) #t]
      [else              (isincluded a (cdr lis))])))




;;; Test Suite for Project 2

;; Validates that test evaluates to correct output. Throws an error if it does not.
(define test
  (lambda (val file)
    (cond
      [(not (eq? val (main file))) (error "TEST FAILED" file)])))

;; Validates that a test throws the correct error. 
(require racket/exn) ; required library for converting to exception to String
(define testError
  (lambda (err file)
    (cond
      [(with-handlers ([exn:fail?
                        (lambda (exn)
                          (not (string-contains? (exn->string exn) err)))])
         (main file))
       (error "TEST FAILED" file)])))

;; Project 1 Tests
;(test 150 "../testfiles/1.txt")
;(test -4 "../testfiles/2.txt")
;(test 10 "../testfiles/3.txt")
;(test 16 "../testfiles/4.txt")
;(test 220 "../testfiles/5.txt")
;(test 5 "../testfiles/6.txt")
;(test 6 "../testfiles/7.txt")
;(test 10 "../testfiles/8.txt")
;(test 5 "../testfiles/9.txt")
;(test -39 "../testfiles/10.txt")
;(testError "y: Used Before Declared" "../testfiles/11.txt")
;(testError "x: Used Before Declared" "../testfiles/12.txt")
;(testError "x: Use Before Assigning" "../testfiles/13.txt")
;(testError "x: Redefining a variable" "../testfiles/14.txt")
;(test 'true "../testfiles/15.txt")
;(test 100 "../testfiles/16.txt")
;(test 'false "../testfiles/17.txt")
;(test 'true "../testfiles/18.txt")
;(test 128 "../testfiles/19.txt")
;(test 12 "../testfiles/20.txt")

; Project 2 Tests
;(test 20 "../testfiles/2-1.txt")
;(test 164 "../testfiles/2-2.txt")
;(test 32 "../testfiles/2-3.txt")
;(test 2 "../testfiles/2-4.txt")
;(testError "min: Used Before Declared" "../testfiles/2-5.txt")
;(test 25 "../testfiles/2-6.txt")
;(test 21 "../testfiles/2-7.txt")
;(test 6 "../testfiles/2-8.txt")
;(test -1 "../testfiles/2-9.txt")
;(test 789 "../testfiles/2-10.txt")
;(testError "y: Used Before Declared" "../testfiles/2-11.txt")
;(testError "a: Used Before Declared" "../testfiles/2-12.txt")
;(testError "Break Outside of Loop" "../testfiles/2-13.txt")
;(test 12 "../testfiles/2-14.txt")
;(test 125 "../testfiles/2-15.txt")
;(test 110 "../testfiles/2-16.txt")
;(test 2000400 "../testfiles/2-17.txt")
;(test 101 "../testfiles/2-18.txt")
;(testError "Throw Outside of try" "../testfiles/2-19.txt")

; Project 3 Tests
(test 10 "../testfiles/3-1.txt")
(test 14 "../testfiles/3-2.txt")
(test 45 "../testfiles/3-3.txt")
(test 55 "../testfiles/3-4.txt")
(test 1 "../testfiles/3-5.txt")
(test 115 "../testfiles/3-6.txt")
(test 'true' "../testfiles/3-7.txt")
(test 20 "../testfiles/3-8.txt")
(test 24 "../testfiles/3-9.txt")
(test 2 "../testfiles/3-10.txt")
(test 35 "../testfiles/3-11.txt")
(testError "..." "../testfiles/3-12.txt")
(test 90 "../testfiles/3-13.txt")
(test 69 "../testfiles/3-14.txt")
(test 87 "../testfiles/3-15.txt")
(test 64 "../testfiles/3-16.txt")
(testError "..." "../testfiles/3-17.txt")
(test 125 "../testfiles/3-18.txt")
(testError "..." "../testfiles/3-19.txt")
(test 2000400 "../testfiles/3-20.txt")









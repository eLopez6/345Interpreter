#lang racket
;Mark Goldberg
;Emilio Lopez
;Jacob Alspaw

;(require "simpleParser.rkt")
;(require "functionParser.rkt")
(require "classParser.rkt")

;; Definitions for abstraction
(define emptyclasslist (lambda () '(()())))
(define emptyenviro (lambda () '(()())))
(define emptystate (lambda () '(()())))
(define startcatch '())
(define startfinally '())
(define startcontinue '())
(define startbreak '())


;; Main function
(define main
  (lambda (mainclass filename)
    (checkbool
     (call/cc
      (lambda (k)
        (invokemain mainclass (classclosures (parser filename) (emptyclasslist)) k))))))


;; Invokes the main method of the requested class
(define invokemain
  (lambda (mainclass classlist mainreturn)
    (cond
      [(null? (car classlist))
       (error mainclass "The class does not exist in the defined classes")]
      [(eq? mainclass (caar classlist))
       (mainreturn (car (runstaticfunction 'main
                                           (lookup-all 'main (car (cdaadr classlist)))
                                           '()
                                           (environmentformain (caadr classlist))
                                           classlist
                                           startcatch
                                           startfinally)))]
      [else
       (invokemain mainclass (cons
                              (cdar (classlist))
                              (list (cdadr classlist))))])))


;; environment for main function
(define environmentformain
  (lambda (classclosure)
    (list (cons (list 'main) (list (lookup-all 'main (cadr classclosure)))))))


;; create closure for all class
(define classclosures
  (lambda (lis classlist)
    (cond
      [(null? lis) classlist]
      [else        (classclosures (cdr lis) (appendclass (car lis) classlist))])))


;; add individual class
(define appendclass
  (lambda (lis classlist)
    (cond
      [(checkexists (classname lis)  classlist)
       (error (classname lis) "Redefining a class")]
      [else
       (cons
        (cons (classname lis) (car classlist))
        (list (cons (list (classparent lis) (createclassclosure lis)) (cadr classlist))))])))


;; returns class closure for defined class
(define createclassclosure
  (lambda (lis)
    (menvironment (classbody lis) (list (emptyenviro)))))


(define classname
  (lambda (lis)
    (cadr lis)))

(define classparent
  (lambda (lis)
    (cond
      [(null? (caddr lis)) '()]
      [else                (cadr (caddr lis))])))

(define classbody
  (lambda (lis)
    (cadddr lis)))



;; converts a true/false value to full representation
;; otherwise returns the value
(define checkbool
  (lambda (val)
    (cond
      [(eq? val #t)   'true]
      [(eq? val #f)   'false]
      [else           val])))


;;executes the global space and then calls main
(define menvironment
  (lambda (lis enviro)
    (cond
      [(null? lis)
       enviro]
      [(or (eq? (caar lis) 'function) (eq? (caar lis) 'static-function))
       (menvironment (cdr lis) (createfunction (car lis) enviro))]
      [(eq? (caar lis) 'var)
       (menvironment (cdr lis) (instantiatevar (cdar lis) enviro (emptyclasslist) startcatch startfinally))])))


;;returns the function name
(define functionname
  (lambda (lis)
    (cadr lis)))


;;returns the list of function params
(define functionparam
  (lambda (lis)
    (caddr lis)))


;;returns the parsed function body
(define functionbody
  (lambda (lis)
    (cadddr lis)))


;;adds the function name and its closure to the state
(define createfunction
  (lambda (lis state)
    (cond
      [(checkexists (functionname lis)  (car state))
       (error (car lis) "Redefining a function")]
      [else
       (cons (cons
              (cons (functionname lis) (caar state))
              (list (cons (createfunctionclosure lis) (cadar state))))
             (cdr state))])))

;;creates closure of function
(define createfunctionclosure
  (lambda (lis)
    (list (functionparam lis) (functionbody lis) getfunctionstate)))


;;a function that returns the state used by the function
;;the state used by the function should include a state of its params and any state that existed before the function declaration
;;only on instances
(define getfunctionstate
  (lambda (instancename functionname paramnames paramvals state classlist catch finally)
    (if (eq? (len paramnames) (len paramvals))
        (cond
          [(eq? instancename 'this)
           (cons (list paramnames (getparamval paramvals state classlist catch finally))
                 (stateatfunctiondeclared functionname state))]
          [(eq? instancename 'super)
           (cons (list paramnames (getparamval paramvals state classlist catch finally))
                 (stateatfunctiondeclared functionname (lookup-all 'this state)))]
          [(not (list? instancename))
           (cons (list paramnames (getparamval paramvals state classlist catch finally))
                 (stateatfunctiondeclared functionname (lookup-all instancename state)))]
          [else
           (let [(newstate (evaluate instancename state classlist catch finally))]
             (cons (list paramnames (getparamval paramvals state classlist catch finally))
                   (stateatfunctiondeclared functionname newstate)))])
        (error "Mismatched parameters and arguments"))))


;;returns the state at which the function was declared and any state that existed before the function declaration
(define stateatfunctiondeclared
  (lambda (name state)
    (cond
      [(null? (cdr state))
       state]
      [(checkexists name (car state))
       state]
      [else
       (stateatfunctiondeclared name (cdr state))])))


;;returns any state that was created after the function declaration
(define statebeforefunctiondeclared
  (lambda (instancename state)
    (if (checkexists instancename (car state))
        '()
        (cons (car state) (statebeforefunctiondeclared instancename (cdr state))))))


;;creates a list of parameters and their corresponding values
(define getparamval
  (lambda (lis state classlist catch finally)
    (cond
      [(null? lis) '()]
      [else        (cons (evaluate (car lis) state classlist catch finally) (getparamval (cdr lis) state classlist catch finally))])))


;;runs a static function
(define runstaticfunction
  (lambda (funcname closure paramvals state classlist catch finally)
    (call/cc
     (lambda (startreturn)
       (mstate (cadr closure)
               (list (emptystate))
               classlist
               catch
               finally
               startcontinue
               startbreak
               startreturn)))))

;;runs a function of a
(define runinstancefunction
  (lambda (instancename funcname paramvals state classlist catch finally)
    (cond
      [(not (list? instancename)) 
       (let [(closure (lookupinstancevar instancename funcname state))]
         (updatestateafterfunctioncall instancename
                                       funcname
                                       state
                                       (call/cc
                                        (lambda (startreturn)
                                          (mstate (cadr closure)
                                                  ((caddr closure) instancename funcname (car closure) paramvals state classlist catch finally)
                                                  classlist
                                                  catch
                                                  finally
                                                  startcontinue
                                                  startbreak
                                                  startreturn)))))]
      [else
       (let [(closure (lookupinstancevar 'this funcname (newclassinstance (cadr instancename) classlist)))]
         (call/cc
          (lambda (startreturn)
            (mstate (cadr closure)
                    ((caddr closure) instancename funcname (car closure) paramvals state classlist catch finally)
                    classlist
                    catch
                    finally
                    startcontinue
                    startbreak
                    startreturn))))])))


;combines the returned state with the state that existed before the function was called
(define updatestateafterfunctioncall
  (lambda (instancename funcname oldstate newstate)
    (cond
      [(not (list? (car newstate)))
       (cons (car newstate) (updateinstance instancename (cdr newstate) oldstate))]
      [else
       (cons '() (updateinstance instancename (cdr newstate) oldstate))])))


;;changes state of instance
(define updateinstance
  (lambda (instancename newstate oldstate)
    (updateinstance-all-acc instancename newstate oldstate '())))


;; update class instance
;;new state can't be evaluated, need to make own function for updating instance
(define updateinstance-all-acc
  (lambda (instancename instancevalue state acc)
    (cond
      [(null? state)
       (error instancename "Used before declared or out of scope")]
      [(checkexists instancename (car state))
       (append acc (cons (updateinstance-acc instancename
                                             instancevalue
                                             (car state)
                                             (emptystate))
                         (cdr state)))]
      [else
       (updateinstance-all-acc instancename
                               instancevalue
                               (cdr state)
                               (append acc (list (car state))))])))


;; Set variable for single state
(define updateinstance-acc
  (lambda (instancename instancevalue state acc)
    (cond
      [(null? (car state)) acc]
      [(eq? (caar state) instancename)
       (updateinstance-acc instancename
                           instancevalue
                           (cons (cdar state)
                                 (list (cdadr state)))
                           (cons (cons (caar state)
                                       (car acc))
                                 (list (cons instancevalue
                                             (cadr acc)))))]
      [else
       (updateinstance-acc instancename
                           instancevalue
                           (cons (cdar state)
                                 (list (cdadr state)))
                           (cons (cons (caar state)
                                       (car acc))
                                 (list (cons (caadr state)
                                             (cadr acc)))))])))
  


;; Mstate
(define mstate
  (lambda (lis state classlist catch finally continue break return)
    (cond
      [(null? lis) state]
      
      [(eq? (caar lis) 'return)
       (return (cons (evaluate (debug (cadar lis)) (debug state) classlist catch finally) state))]
      
      [(eq? (caar lis) 'var)
       (mstate (cdr lis)
               (instantiatevar (cdar lis) state classlist catch finally)
               classlist
               catch finally continue break return)]
      
      [(eq? (caar lis) '=)
       (cond
         [(list? (cadar lis))
          (mstate (cdr lis)
                  (updateinstancevar (cdar lis) state classlist catch finally)
                  classlist
                  catch finally continue break return)]
         [else
          (mstate (cdr lis)
                  (updatevar (cdar lis) state classlist catch finally)
                  classlist
                  catch finally continue break return)])]
      
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
      
      [(and (eq? (caar lis) 'throw) (not (null? catch)))
       (catch (cons (list (evaluate (cadar lis) state catch finally)) state))]
      
      [(eq? (caar lis) 'throw)
       (error "Throw Outside of try")]
      
      [(eq? (caar lis) 'funcall)
       (mstate (cdr lis)
               (cdr (runinstancefunction (car (cdadar lis)) (cadr (cdadar lis)) (cddar lis) state classlist catch finally))
               classlist
               catch finally continue break return)]

      [(eq? (caar lis) 'function)
       (mstate (cdr lis) (createfunction (car lis) state) catch finally continue break return)])))    


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
  (lambda (trylis catchlis finallylis state classlist catch finally continue break return)
    (evalfinally finallylis
                 (call/cc
                  (lambda (finally)
                    (evalcatch catchlis
                               (call/cc
                                (lambda (catch)
                                  (evaltry trylis
                                           state classlist catch finally continue break return))) ; conts for evaltry
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
  (lambda (catchlis state classlist catch finally continue break return)
    (if (null? catchlis)
        state
        (finally (removestatelayer (mstate (caddr catchlis)
                                           (instantiatevar
                                            (append (cadr catchlis) (car state))
                                            (addstatelayer (cdr state))
                                            classlist
                                            catch
                                            finally)
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
  (lambda (lis state classlist catch finally)
    (cond
      [(or (boolean? lis) (number? lis))
       lis]
      [(not (list? lis))
       (lookup-all lis state)]
      [(isincluded (operator lis) '(+ - * / %))
       (mvalue lis state classlist catch finally)]
      [(isincluded (operator lis) '(> >= < <= == || && !))
       (mbool lis state catch finally)]
      [(eq? (operator lis) 'funcall)
       (car (runinstancefunction (cadadr lis) (car (cddadr lis)) (cddr lis) state classlist catch finally))]
      [(eq? (operator lis) 'new)
       (newclassinstance (cadr lis) classlist)]
      [(and (eq? (operator lis) 'dot) (not (list? (operand1 lis))))
       (lookupinstancevar (operand1 lis) (operand2 lis) state)]
      [(and (eq? (operator lis) 'dot) (list? (operand1 lis)))
       (let [(newstate (evaluate (operand1 lis) state classlist catch finally))]
         (lookupinstancevar 'this (operand2 lis) newstate))])))


;; look up variable in an instance of a class
(define lookupinstancevar
  (lambda (instancename var state)
    (cond
      [(eq? instancename 'this)
       (lookupinstancevarhelper var (lookup-all instancename state))]
      [(eq? instancename 'super)
       (lookupinstancevarhelper var (cdr (lookup-all 'this state)))]
      [else
       (lookupinstancevarhelper var (lookup-all 'this (lookup-all instancename state)))])))

(define lookupinstancevarhelper
  (lambda (var instance)
    (cond
      [(null? instance)
       (error var "Function / field not defined")]
      [(null? (lookup-all var instance))
       (lookupinstancevarhelper var (cdr instance))]
      [else
       (lookup-all var instance)])))


;; update instance variable
(define updateinstancevar
  (lambda (lis state classlist catch finally)
    (let [(values (cons (caddar lis) (list (evaluate (cadr lis) state classlist catch finally))))]
      (updateinstance
       'this
       (updatevar values (lookup-all 'this state) classlist catch finally)
       state))))
  


(define newclassinstance
  (lambda (classname classlist)
    (list (cons (list 'this) (list (list (newclassinstancehelper classname classlist)))))))

;;creates a new instance of a class
(define newclassinstancehelper
  (lambda (classname classlist)
    (cond
      [(null? classname)
       '()]
      [else
       (let [(closure (getclassclosure classname classlist))]
         (cons (caadr closure) (newclassinstancehelper (car closure) classlist)))])))


;; looks for class
(define getclassclosure
  (lambda (classname classlist)
    (lookup classname classlist identity)))



;; Mvalue for numeric operations
(define mvalue
  (lambda (lis state classlist catch finally)
    ; f makes the code more readable, since the behavior is almost always (operator (operands)
    ; f applies the input operator the mvalue of the expressions (same as in mvalue above)
    (let [(f (lambda (op)
               (op (evaluate (operand1 lis) state classlist catch finally)
                   (evaluate (operand2 lis) state classlist catch finally))))]
      (cond
        [(number? lis)
         lis]
        [(not (list? lis))
         (lookup-all lis state)]
        [(eq? (operator lis) '+)      (f +)]
        [(and (eq? (operator lis) '-)
              (eq? (len lis) 3))
         (f -)]
        [(eq? (operator lis) '-)      (- (mvalue (operand1 lis) state classlist catch finally))] ; negation
        [(eq? (operator lis) '*)      (f *)]
        [(eq? (operator lis) '/)      (f quotient)]
        [(eq? (operator lis) '%)      (f remainder)]))))


;; Mbool for boolean logic
(define mbool
  (lambda (lis state catch finally)
    ; f and b just make the code more readable
    ; f applies the input operator the mvalue of the expressions (same as in mvalue above)
    ; b applies the input comparison to the mbool of the expression
    (let [(f (lambda (op)
               (op (evaluate (operand1 lis) state catch finally)
                   (evaluate (operand2 lis) state catch finally))))
          (b (lambda (op)
               (op (evaluate (operand1 lis) state catch finally)
                   (evaluate (operand2 lis) state catch finally))))]
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
        [(eq? (operator lis) '!)  (not (mbool (operand1 lis) state catch finally))]))))


;; for if statements
(define mif
  (lambda (lis state catch finally continue break return)
    (cond
      [(mbool (cadr lis) state catch finally)
       (mstate (list (caddr lis)) state catch finally continue break return)]
      [(hasNestedIf lis)
       (mif (cadddr lis) state catch finally continue break return)]
      [else
       (mstate (cdddr lis) state catch finally continue break return)])))


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
      [(mbool (cadr lis) state catch finally)
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
  (lambda (lis state classlist catch finally)
    (cond
      [(checkexists (car lis) (car state))
       (error (car lis) "Redefining a variable")]
      [(null? (cdr lis))
       (addtostate (car lis) state)]
      [else
       (updatevar lis (addtostate (car lis) state) classlist catch finally)])))


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
  (lambda (lis state classlist catch finally)
    (updatevar-all-acc lis state state classlist '() catch finally)))


;; set variable in most recent state for list of states
(define updatevar-all-acc
  (lambda (lis state all-state classlist acc catch finally)
    (cond
      [(null? state)
       (error (car lis) "Used before declared or out of scope")]
      [(checkexists (car lis) (car state))
       (append acc (cons (updatevar-acc (car lis)
                                        (evaluate (cadr lis) all-state classlist catch finally)
                                        (car state)
                                        (emptystate))
                         (cdr state)))]
      [else
       (updatevar-all-acc lis
                          (cdr state)
                          all-state
                          classlist
                          (append acc (list (car state)))
                          catch
                          finally)])))


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
          (error var "Used Before Declared")])))))

      
;; Checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis)       #f]
      [(eq? (car lis) a) #t]
      [else              (isincluded a (cdr lis))])))







;;debug from lecture
(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((x ,(cadr slist)))
                          (begin (print x) (newline) x)))))


;; Validates that test evaluates to correct output. Throws an error if it does not.
(define test
  (lambda (mainclass val file)
    (cond
      [(eq? val (main mainclass file)) (println (string-append "passed --> " file))]
      [else (error "TEST FAILED" file)])))

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
;(testError "y: Used before declared or out of scope" "../testfiles/11.txt")
;(testError "x: Used before declared or out of scope" "../testfiles/12.txt")
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
;(testError "min: Used before declared or out of scope" "../testfiles/2-5.txt")
;(test 25 "../testfiles/2-6.txt")
;(test 21 "../testfiles/2-7.txt")
;(test 6 "../testfiles/2-8.txt")
;(test -1 "../testfiles/2-9.txt")
;(test 789 "../testfiles/2-10.txt")
;(testError "y: Used before declared or out of scope" "../testfiles/2-11.txt")
;(testError "a: Used before declared or out of scope" "../testfiles/2-12.txt")
;(testError "Break Outside of Loop" "../testfiles/2-13.txt")
;(test 12 "../testfiles/2-14.txt")
;(test 125 "../testfiles/2-15.txt")
;(test 110 "../testfiles/2-16.txt")
;(test 101 "../testfiles/2-18.txt")
;(testError "Throw Outside of try" "../testfiles/2-19.txt")

; Project 3 Tests
;(test 10 "../testfiles/3-1.txt")
;(test 14 "../testfiles/3-2.txt")
;(test 45 "../testfiles/3-3.txt")
;(test 55 "../testfiles/3-4.txt")
;(test 1 "../testfiles/3-5.txt")
;(test 115 "../testfiles/3-6.txt")
;(test 'true' "../testfiles/3-7.txt")
;(test 20 "../testfiles/3-8.txt")
;(test 24 "../testfiles/3-9.txt")
;(test 2 "../testfiles/3-10.txt")
;(test 35 "../testfiles/3-11.txt")
;(testError "Mismatched parameters and arguments" "../testfiles/3-12.txt")
;(test 90 "../testfiles/3-13.txt")
;(test 69 "../testfiles/3-14.txt")
;(test 87 "../testfiles/3-15.txt")
;(test 64 "../testfiles/3-16.txt")
;(testError "b: Used before declared or out of scope" "../testfiles/3-17.txt")
;(test 125 "../testfiles/3-18.txt")
;(test 100 "../testfiles/3-19.txt")
;(test 2000400 "../testfiles/3-20.txt")


; Project 3 Tests
(test 'A 5 "../testfiles/4-0.txt")
(test 'B 5 "../testfiles/4-0.5.txt")
(test 'A 15 "../testfiles/4-1.txt")
(test 'A 12 "../testfiles/4-2.txt")
(test 'A 125 "../testfiles/4-3.txt")
(test 'A 36 "../testfiles/4-4.txt")
(test 'A 54 "../testfiles/4-5.txt")
(test 'A 110 "../testfiles/4-6.txt")
(test 'C 26 "../testfiles/4-7.txt")









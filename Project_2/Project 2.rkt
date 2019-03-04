#lang racket
(require "simpleParser.rkt")

;;where it starts
(define main
  (lambda (filename)
    (call/cc
      (lambda (k)
        (mstate (parser filename)
                '(()())
                '()
                (lambda (val) (k (if (number? val) val (if val 'true 'false)))))))))


(define mstate
  (lambda (lis state break return)
    (cond
      [(null? lis) state]
      [(eq? (caar lis) 'return) (return (evaluate (cadar lis) state))]
      [(eq? (caar lis) 'var)    (mstate (cdr lis) (instantiatevar (car lis) state) break return)]
      [(eq? (caar lis) '=)      (mstate (cdr lis) (updatevar (cdar lis) state) break return)]
      [(eq? (caar lis) 'if)     (mstate (cdr lis) (mif (car lis) state break return) break return)]
      [(eq? (caar lis) 'while)  (mstate (cdr lis) (call/cc (lambda (break) (mwhile (car lis) state break return))) break return)]
      [(eq? (caar lis) 'break)  (break state)]
      [(eq? (caar lis) 'begin)  ])));(mstate (cdr lis) ]))))

(define addstatelayer
  (lambda (state)
    (cons '(()()) (list state))))

(define removestatelayer
  (lambda (state)
    (cdr state)))



(define evaluate
  (lambda (lis state)
    (cond
      [(or (boolean? lis) (number? lis))              lis]
      [(not (list? lis))                              (lookup lis state)]
      [(isincluded (car lis) '(+ - * / %))            (mvalue lis state)]
      [(isincluded (car lis) '(> >= < <= == || && !)) (mbool lis state)])))
    


;;mvalue for math
(define mvalue
  (lambda (lis state)
    (cond
      [(number? lis)
       lis]
      [(not (list? lis))
       (lookup lis state)]
      [(eq? (operator lis) '+)
       (+ (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(and (eq? (operator lis) '-) (eq? (len lis) 3))
       (- (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))];subtractions
      [(eq? (operator lis) '-)
       (- (mvalue (operand1 lis) state))];negation
      [(eq? (operator lis) '*)
       (* (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '/)
       (quotient (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '%)
       (remainder (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))])))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)


;;mbool for boolean logic
(define mbool
  (lambda (lis state)
    (cond
      [(boolean? lis)           lis]
      [(not (list? lis))        (lookup lis state)]
      [(eq? (operator lis) '>)  (> (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '>=) (>= (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '<)  (< (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '<=) (<= (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '==) (eq? (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '!=) (not (eq? (mvalue (operand1 lis) state) (mvalue (operand2 lis) state)))]
      [(eq? (operator lis) '&&) (and (mbool (operand1 lis) state) (mbool (operand2 lis) state))]
      [(eq? (operator lis) '||) (or (mbool (operand1 lis) state) (mbool (operand2 lis) state))]
      [(eq? (operator lis) '!)  (not (mbool (operand1 lis) state))])))




;;for if statements
(define mif
  (lambda (lis state break return)
    (cond
      [(mbool (cadr lis) state) (mstate (list (caddr lis)) state break return)]
      [(hasNestedIf lis)        (mif (cadddr lis) break state)]
      [else                     (mstate (cdddr lis) state break return)])))

;;checks if there's and else if
(define hasNestedIf
  (lambda (lis)
    (and (>= (len lis) 4) (eq? (car (cadddr lis)) 'if))))


;;calc length using accumulator
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
  (lambda (lis state break return)
    (cond
      [(mbool (cadr lis) state) (mwhile lis (mstate (cddr lis) state break return) break return)]
      [else                     state])))



;;instatiate variables
(define instantiatevar
  (lambda (lis state)
    (cond
      [(null? (cddr lis))             (cons (cons (cadr lis) (car state)) (list (cons '() (cadr state))))]
      [(checkexists (cadr lis) state) (error (cadr lis) "Redefing a variable")] 
      [else                           (updatevar (cdr lis) (cons (cons (cadr lis) (car state)) (list (cons '() (cadr state)))))])))


; Check if a variable already has been declared
(define checkexists
  (lambda (var state)
    (cond
      [(null? (car state))    #f]
      [(eq? (caar state) var) #t]
      [else                   (checkexists var (cons (cdar state) (list (cdadr state))))])))


;;default use of updatevar
(define updatevar
  (lambda (lis state)
    (updatevar-acc (car lis) (evaluate (cadr lis) state) state '(()()))))

;;set variable
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
  (lambda (var state)
    (cond
      [(eq? var 'true)                                    #t]
      [(eq? var 'false)                                   #f] 
      [(null? (car state))                                (error var "Used Before Declared")]
      [(and (eq? (caar state) var) (null? (caadr state))) (error var "Use Before Assigning")]
      [(eq? (caar state) var)                             (caadr state)]
      [else                                               (lookup var (cons (cdar state) (list (cdadr state))))])))



;;checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis)       #f]
      [(eq? (car lis) a) #t]
      [else              (isincluded a (cdr lis))])))


(main "../testfiles/1.txt")
(main "../testfiles/2.txt")
(main "../testfiles/3.txt")
(main "../testfiles/4.txt")
(main "../testfiles/5.txt")
(main "../testfiles/6.txt")
(main "../testfiles/7.txt")
(main "../testfiles/8.txt")
(main "../testfiles/9.txt")
(main "../testfiles/10.txt")
;(main "../testfiles/11.txt")
;(main "../testfiles/12.txt")
;(main "../testfiles/13.txt")
;(main "../testfiles/14.txt")
(main "../testfiles/15.txt")
(main "../testfiles/16.txt")
(main "../testfiles/17.txt")
(main "../testfiles/18.txt")
(main "../testfiles/19.txt")
(main "../testfiles/20.txt")
(main "../testfiles/2-9.txt")









#lang racket
(require "simpleParser.rkt")

;;where it starts
(define main
  (lambda (filename)
    (mstate (parser filename) '())))


;;
(define mstate
  (lambda (lis state)
    (cond
      [(null? lis) (return state)]
      [(number? (car lis)) (car lis)]
      [(boolean? (car lis)) (car lis)]
      [(eq? (caar lis) 'return) (mstate (cdr lis) (addreturn state (cdr lis)))]
      [(eq? (caar lis) 'var) (mstate (cdr lis) (instantiatevar (car lis) state))]
      [(eq? (caar lis) '=) (mstate (cdr lis) (updatevar (car lis) state))]
      [(eq? (caar lis) 'if) (mstate (cdr lis) (mif (car lis) state))]
      [(isincluded (caar lis) '(+ - * / %)) (mvalue (car lis))]
      [(isincluded (caar lis) '(> >= < <= ==)) (mbool (car lis))])))


;;gets return val
(define return
  (lambda (state)
    (cond
      [(null? state) 'error]
      [(eq? (caar state) 'return) (car (cdr (car state)))]
      [else (return (cdr state))])))

(define addreturn
  (lambda (state val)
    (append state (list (cons 'return (mstate val state))))))
    


;;mvalue for math
(define mvalue
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(not (list? lis)) (lookup lis state)]
      [(eq? (operator lis) '+) (+ (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '-) (- (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '*) (* (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '/) (quotient (mvalue (operand1 lis)) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '%) (remainder (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))])))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)



;;mbool for boolean logic
(define mbool
  (lambda (lis state)
    (cond
      [(boolean? lis) lis]
      [(not (list? lis)) (lookup lis state)]
      [(eq? (operator lis) '>) (> (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '>=) (>= (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '<) (< (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '<=) (<= (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '==) (eq? (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '!=) (not (eq? (mvalue (operand1 lis) state) (mvalue (operand2 lis) state)))]
      [(eq? (operator lis) '&&) (and (mbool (operand1 lis) state) (mbool (operand2 lis) state))]
      [(eq? (operator lis) '||) (or (mbool (operand1 lis) state) (mbool (operand2 lis) state))]
      [(eq? (operator lis) '!) (not (mbool (operand1 lis) state))])))




;;for if statements
(define mif
  (lambda (lis state)
    (cond
      [(mbool (cadr lis) state) (mstate (list (caddr lis)) state)]
      [(hasNestedIf lis) (mif (cadddr lis) state)])))

;;checks if there's and else if
(define hasNestedIf
  (lambda (lis)
    (>= (len lis) 4)))


;;calc length using accumulator
(define len-acc
  (lambda (lis acc)
    (if (null? lis)
        acc
        (len-acc (cdr lis) (+ 1 acc)))))

(define len
  (lambda (lis)
    (len-acc lis 0)))



;;implementation of while loops      ******untested (needs vars)
(define mwhile
  (lambda (lis state)
    (cond
      [(mbool (cadr lis)) (mwhile (mstate (caddr lis) state) '())])))



;;instatiate variables
(define instantiatevar
  (lambda (lis state)
    (cons (list (cadr lis)) state)))


;;default use of updatevar
(define updatevar
  (lambda (lis state)
    (updatevar-acc lis state '())))

;;set variable
(define updatevar-acc
  (lambda (lis state acc)
    (cond
      [(null? state) acc]
      [(eq? (caar state) (cadr lis)) (updatevar-acc lis (cdr state) (append acc (list (cons (cadr lis) (list (caddr lis))))))]
      [else (updatevar-acc lis (cdr state) (append acc (list (car state))))])))


;;finds saved value of var
(define lookup
  (lambda (var state)
    (cond
      [(null? state) 'error]
      [(eq? (caar state) var) (cadar state)]
      [else (lookup var (cdr state))])))



;;checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? (car lis) a) #t]
      [else (isincluded a (cdr lis))])))
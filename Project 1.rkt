#lang racket
(require "simpleParser.rkt")

;;where it starts
(define main
  (lambda (filename)
    (m (parser filename))))

;;recurses after gets pared file
(define m
  (lambda (lis)
    (cond
      [(isincluded (car lis) '(+ - * / %)) (mvalue lis)]
      [(isincluded (car lis) '(> >= < <= ==)) (mbool lis)]
      )))

(define mstate
  (lambda (lis)
    5))



;;mvalue for math
(define mvalue
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? (operator lis) '+) (+ (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '-) (- (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '*) (* (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '/) (quotient (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '%) (remainder (mvalue (operand1 lis)) (mvalue (operand2 lis)))])))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)



;;mbool for boolean logic
(define mbool
  (lambda (lis)
    (cond
      [(or (eq? lis #t) (eq? lis #f)) lis]
      [(eq? (operator lis) '>) (> (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '>=) (>= (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '<) (mbool '(!(> 5 (5 + 5))))(< (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '<=) (<= (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '==) (eq? (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '!=) (not (eq? (mvalue (operand1 lis)) (mvalue (operand2 lis))))]
      [(eq? (operator lis) '&&) (and (mbool (operand1 lis)) (mbool (operand2 lis)))]
      [(eq? (operator lis) '||) (or (mbool (operand1 lis)) (mbool (operand2 lis)))]
      [(eq? (operator lis) '!) (not (mbool (operand1 lis)))])))



;;for if statements
(define mif
  (lambda (lis)
    (cond
      [(mbool (cadr lis)) (caddr lis)]
      [(hasNestedIf lis) (mif (cadddr lis))])))

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
  (lambda (lis)
    (cond
      [(mbool (cadr lis)) (mwhile (mstate (caddr lis)))])))



;;checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? (car lis) a) #t]
      [else (isincluded a (cdr lis))])))
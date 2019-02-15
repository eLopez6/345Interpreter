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
      )))


;;mvalue for math
(define mvalue
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? (operator lis) '+) (+ (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '-) (- (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '*) (* (mvalue (operand1 lis)) (mvalue (operand2 lis)))]
      [(eq? (operator lis) '/) (quotient (mvalue (operand1 lis)) (mvalue operand2 lis))]
      [(eq? (operator lis) '%) (remainder (mvalue (operand1 lis)) (mvalue operand2 lis))])))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)


;;checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? (car lis) a) #t]
      [else (isincluded a (cdr lis))])))
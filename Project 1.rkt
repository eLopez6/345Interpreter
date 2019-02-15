#lang racket
require "simpleParser.rkt"

(define main
  (lambda (lis)
    (cond

      )))



;;this should help
(define mvalue3
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? (operator lis) '+) (+ (mvalue3 (operand1 lis)) (mvalue3 (operand2 lis)))]
      [(eq? (operator lis) '-) (- (mvalue3 (operand1 lis)) (mvalue3 (operand2 lis)))]
      [(eq? (operator lis) '*) (* (mvalue3 (operand1 lis)) (mvalue3 (operand2 lis)))]
      [(eq? (operator lis) '/) (quotient (mvalue3 (operand1 lis)) (mvalue3 operand2 lis))]
      [(eq? (operator lis) '%) (remainder (mvalue3 (operand1 lis)) (mvalue3 operand2 lis))])))
#lang racket
(require "simpleParser.rkt")

;;where it starts
(define main
  (lambda (filename)
    (return (mstate (parser filename) '((true #t) (false #f))))))


;;
(define mstate
  (lambda (lis state)
    (cond
      [(null? lis) state]
      [(eq? (caar lis) 'return) (mstate (cdr lis) (addreturn state (cdar lis)))]
      [(eq? (caar lis) 'var) (mstate (cdr lis) (instantiatevar (car lis) state))]
      [(eq? (caar lis) '=) (mstate (cdr lis) (updatevar (car lis) state))]
      [(eq? (caar lis) 'if) (mstate (cdr lis) (mif (car lis) state))]
      [(eq? (caar lis) 'while) (mstate (cdr lis) (mwhile (car lis) state))])))


(define evaluate
  (lambda (lis state)
    (cond
      [(or (boolean? lis) (number? lis)) lis]
      [(not (list? lis)) (lookup lis state)]
      [(isincluded (car lis) '(+ - * / %)) (mvalue lis state)]
      [(isincluded (car lis) '(> >= < <= == || && !)) (mbool lis state)])))


;;gets return val
(define return
  (lambda (state)
    (cond
      [(null? state) (error "No Return")]
      [(and (eq? (caar state) 'return) (eq? #t (cadar state))) 'true]
      [(and (eq? (caar state) 'return) (eq? #f (cadar state))) 'false]
      [(eq? (caar state) 'return) (cadar state)]
      [else (return (cdr state))])))

(define addreturn
  (lambda (state val)
    (cond
      [(or (boolean? (car val)) (number? (car val))) (append state (list (cons 'return val)))];is a bool or a number
      [(not (list? (car val))) (append state (list (cons 'return (list (lookup (car val) state)))))];is a var
      [else (append state (list (cons 'return (list (evaluate (car val) state)))))])))
    


;;mvalue for math
(define mvalue
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(not (list? lis)) (lookup lis state)]
      [(eq? (operator lis) '+) (+ (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(and (eq? (operator lis) '-) (eq? (len lis) 3)) (- (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))];subtractions
      [(eq? (operator lis) '-) (- (mvalue (operand1 lis) state))];negation
      [(eq? (operator lis) '*) (* (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
      [(eq? (operator lis) '/) (quotient (mvalue (operand1 lis) state) (mvalue (operand2 lis) state))]
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
      [(hasNestedIf lis) (mif (cadddr lis) state)]
      [else (mstate (cdddr lis) state)])))

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
  (lambda (lis state)
    (cond
      [(mbool (cadr lis) state) (mwhile lis (mstate (cddr lis) state))]
      [else state])))



;;instatiate variables
(define instantiatevar
  (lambda (lis state)
    (cond
      [(null? (cddr lis)) (cons (list (cadr lis)) state)]
      [else (updatevar (append (cons '= (list (cadr lis))) (list (evaluate (caddr lis) state))) (cons (list (cadr lis)) state))])))


;;default use of updatevar
(define updatevar
  (lambda (lis state)
    (updatevar-acc lis state '())))

;;set variable
(define updatevar-acc
  (lambda (lis state acc)
    (cond
      [(null? state) acc]
      [(and (or (number? (caddr lis)) (boolean? (caddr lis))) (eq? (caar state) (cadr lis))) (updatevar-acc lis (cdr state) (append acc (list (cons (cadr lis) (list (caddr lis))))))]
      [(eq? (caar state) (cadr lis)) (updatevar-acc lis (cdr state) (append acc (list (cons (cadr lis) (list (evaluate (caddr lis) state))))))]
      [else (updatevar-acc lis (cdr state) (append acc (list (car state))))])))


;;finds saved value of var
(define lookup
  (lambda (var state)
    (cond
      [(null? state) (error var "Used Before Declared")]
      [(and (eq? (caar state) var) (not (eq? 2 (len (car state))))) (error var "Use Before Assigning")]
      [(eq? (caar state) var) (cadar state)]
      [else (lookup var (cdr state))])))



;;checks if atom is the same as any atom in a list (not * for a reason)
(define isincluded
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? (car lis) a) #t]
      [else (isincluded a (cdr lis))])))
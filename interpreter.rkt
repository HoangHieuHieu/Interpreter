
#lang racket
(require "simpleParser.rkt")
;; abstractions
(define operator
  (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)
(define condition cadr)
(define while-body caddr)
(define if-body1 caddr)
(define if-body2 cdddr)
(define name-list car)
(define val-list cadr)
(define first_stmt car)
(define remaining_stmts cdr)
(define init-state '(()()))
;; cdr-state: take a state return that state without its first binding 
(define cdr-state
  (lambda (state)
    (cons (cdr (name-list state)) (list (cdr (val-list state))))))

;; declared?: check if a var is declared
(define declared?
  (lambda (var state)
    (cond
      [(null? (name-list state)) #f]
      [(eq? (car (name-list state)) var) #t]
      [else (declared? var (cdr-state state))])))

(define M_boolean
  (lambda (condition state)
    (cond
      ((null? condition) #t)
      ((eq? condition 'true)   #t)
      ((eq? condition 'false)  #f)
      ((not (or (boolean? condition) (list? condition))) (getVar condition state))
      ((boolean? (car condition))   (return-stmt (car condition) state))
      
      ; boolean operation 
      
      ((eq? (operator condition) '&&)    (and (M_boolean(leftoperand condition) state)   (M_boolean(rightoperand condition) state)))          
      ((eq? (operator condition) '||)    (or (M_boolean(leftoperand condition) state)    (M_boolean(rightoperand condition) state)))     
      ((eq? (operator condition) '!)     (not (M_boolean (leftoperand condition) state)))
     
      ; comparision operator
      ((eq? (operator condition) '<)    (< (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '>)    (> (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '<=)   (< (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '>=)   (> (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '==)   (= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '!=)   (not (= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state))))
      (else (error 'bad-boolean)))))

(define add
  (lambda (var value state)
    (cond
      ((or (null? (name-list state)) (not (declared? var state)))    (cons (cons var (name-list state)) (list (cons value (val-list state)))))
      ((declared? var state)         (add var value (remove var state))))))



(define M_integer
  (lambda (expression state)
    (cond
      ((null? expression) 0)
      ((number? expression) expression)
      ((not (list? expression)) (getVar expression state))
      ((and (null? (cddr expression)) (eq? (operator expression) '+)) (+ 0 (M_value (leftoperand expression) state)))
      ((and (null? (cddr expression)) (eq? (operator expression) '-)) (- 0 (M_value (leftoperand expression) state)))
      ((eq? (operator expression) '+) (+ (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      
      (else (error 'bad-operator)))))

(define getVar
  (lambda (var state)
    (cond
      ((null? (car state)) '(error 'undeclared-variables))
      ((and (eq? var (car (name-list state))) (eq? (car (val-list state)) 'null)) (error 'variables-were-not-assigned-values))
      ((eq? var (car (name-list state))) (car (val-list state)))
      (else (getVar var (cdr-state state))))))


; Take a statement and a state, return the state after execute the statement on the state  
(define M_state
  (lambda (stmt state)
    (cond
      [(null? stmt) state]
      [(list? (operator stmt)) (M_state (remaining_stmts stmt) (M_state (first_stmt stmt) state))]
      [(eq? (operator stmt) 'var) (declare stmt state)]
      [(eq? (operator stmt) '=) (assign stmt state)]
      [(eq? (operator stmt) 'if) (if-stmt stmt state)]
      [(eq? (operator stmt) 'while) (while-stmt stmt state)]
      [(eq? (operator stmt) 'return) (return-stmt stmt state)]
      [else (error 'stmt-not-defined)])))

;; return: return a value
(define return-stmt
  (lambda (stmt state)
    (format-out (M_value (cadr stmt) state))))

;; assign: binding a value to a variable
(define assign
  (lambda (stmt state)
    (if (or (declared? (leftoperand stmt) state) (eq? (operator stmt) 'var))
    (add (leftoperand stmt) (M_value (rightoperand stmt) state) (remove (leftoperand stmt) state))
    (error 'undeclared-variables))))
    
;; helper function for remove
(define remove-cps
  (lambda (var state return)
    (cond
     [(null? (name-list state)) (return init-state)]
     [(eq? (car (name-list state)) var) (return (cdr-state state))]
     [else (remove-cps var (cdr-state state)
                       (lambda (v) (return (list (cons (car (name-list state)) (name-list v)) (cons (car (val-list state)) (val-list v))))))])))
;; remove a var binding out of the state, return the state after the removal
(define remove
  (lambda (var state)
    (remove-cps var state (lambda (v) v))))


;; declare: add a var binding into a state
(define declare
  (lambda (stmt state)
    (if (null? (cddr stmt))
      (add (leftoperand stmt) 'null (remove (leftoperand stmt) state))
      (add (leftoperand stmt) (M_value (rightoperand stmt) state) (remove (leftoperand stmt) state)))))

;; while: perform a while statement
(define while-stmt
  (lambda(stmt state)
    (if (M_boolean (condition stmt) state)
      (while-stmt stmt (M_state (while-body stmt) state))
      state)))

;; if: perform an if statement
(define if-stmt
  (lambda (stmt state)
    (cond
      ((M_boolean (condition stmt) state) (M_state (if-body1 stmt) state))
      ((not (null? (if-body2 stmt))) (M_state (if-body2 stmt) state))
      (else state))))

;; M_value: takes an expression, return the value of it (either a boolean or an integer)
(define M_value
  (lambda (expression state)
    (cond
      [(eq? expression 'true) true]
      [(eq? expression 'false) false]
      [(number? expression) expression]
      [(not (list? expression)) (getVar expression state)]
      [(eq? (operator expression) '=) (M_value (leftoperand expression) (M_state expression state))]
      [(or (eq? (operator expression) '+) (eq? (operator expression) '-) (eq? (operator expression) '*) (eq? (operator expression) '/) (eq? (operator expression) '%))
       (M_integer expression state)]
      [else (M_boolean expression state)])))

;; interpret: Take in a file name and interpret the code in the file
(define interpret
  (lambda (filename)
    (M_state (parser filename) init-state)))

(define format-out
  (lambda (out)
    (cond
      [(number? out) out]
      [(eq? out #t) 'true]
      [else  'false])))

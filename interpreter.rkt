
#lang racket

;; abstractions
(define operator
  (lambda (expression) (car expression)))



(define leftoperand cadr)
(define rightoperand caddr)
(define name-list car)
(define val-list cadr)
;; cdr-state: take a state return that state without its first binding 
(define cdr-state
  (lambda (state)
    (cons (cdr (name-list state)) (list (cdr (val-list state))))))

(define M_boolean
  (lambda (condition state)
    (cond
      ((null? condition) #t)

      ;this part need Prof to verify parsetree structure
      ((and (list? (car condition))    (eq? (cdr condition) '&&))     (and (M_boolean(car condition) state) (M_boolean(cdr (cdr condition)) state)))          
      ((and (list? (car condition))    (eq? (cdr condition) '||))     (or (M_boolean(car condition) state) (M_boolean(cdr (cdr condition)) state)))     
      ((and (eq?   (car condition) '!) (list? (cdr condition)))       (cons [(not (M_boolean(cdr condition) state))] [(M_boolean(cdr (cdr condition)) state)]))
     
      ; comparision operator
      ((eq? (car condition) '<)    (< (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (car condition) '>)    (> (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (car condition) '<=)   (< (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (car condition) '>=)   (> (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (car condition) '==)   (= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (car condition) '!=)   (not (= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state))))
      (else (error 'boolean)))))


(define add
  (lambda (var value state)
    (cond
      ((null? (name-list state))    (cons (cons (name-list state) var) (cons (val-list state) value)))
      ((eq? (car (name-list state)) var)  (add var value (remove (var state))))  
      ; still not find the var yet
      (else (add var value (cons (cdr (name-list state) (cdr (val-list state)))))))))


(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) 0)
      ((number? expression) expression)
      ((and (not (number? expression)) (not (list? expression))) (getVar expression state))
      ((and (null? (cddr expression)) (eq? (operator expression) '+)) (+ 0 (leftoperand expression)))
      ((and (null? (cddr expression)) (eq? (operator expression) '-)) (- 0 (leftoperand expression)))
      ((eq? (operator expression) '+) (+ (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      (else (error 'bad-operator)))))

(define getVar
  (lambda (var state)
    (cond
      ((null? (car state)) 'null)
      ((eq? var (car (car state))) (caadr state))
      (else (getVar var (cons (cdar state) (cons (cdadr state) '())))))))
;; take a statement and a state, return the state after execute the statement on the state  
(define M_state
  (lambda (stmt state)
    (cond
      [(eq? (operator stmt) 'var) state]
      [(eq? (operator stmt) '=) state]
      [(eq? (operator stmt) 'if) state]
      [(eq? (operator stmt) 'while) state]
      [else (error 'stmt-not-defined)])))



(define assign
  (lambda (stmt state)
    add((leftoperand stmt), (M_value (rightoperand stmt)), (remove (leftoperand stmt) state))))
    
;; helper function for remove
(define remove-cps
  (lambda (var state return)
    (cond
     [(null? (name-list state)) (return state)]
     [(eq? (car (name-list state)) var) (return (cdr-state state))]
     [else (remove-cps var (cdr-state state)
                       (lambda (v) (cons (cons (car (name-list state)) (name-list v)) (list (cons (car (val-list state)) (val-list v))))))])))
;; remove a var binding out of the state, return the state after the removal
(define remove
  (lambda (var state)
    (remove-cps var state (lambda (v) v))))


;; declare: add a var binding into a state
(define declare
  (lambda (stmt state)
    (if (null? (cddr stmt)
      (add (leftoperand stmt) 'null (remove (leftoperand stmt) state))
      (add (leftoperand stmt) (rightoperand stmt) (remove (leftoperand stmt) state))))))

;; while: perform a while statement
(define while
  (lambda(con body state)
    (if (M_boolean con)
      (while con body (M_state body state))
      state)))


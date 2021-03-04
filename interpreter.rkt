
#lang racket


(define operator
  (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)
(define name-list car)
(define val-list cadr)
(define cdr-state
  (lambda (state)
    (cons (cdr (name-list state)) (list (cdr (val-list state))))))

(define M_boolean
  (lambda (condition state)
    (cond
      ; cond 1: condition = error
      ((null? condition) #t)
      ((and (list? (car condition)) (eq? (cdr condition) '&&)) (and (M_boolean(car condition) state) (M_boolean(cdr (cdr condition)) state)))          
      ((and (list? (car condition)) (eq? (cdr condition) '||)) (or (M_boolean(car condition) state) (M_boolean(cdr (cdr condition)) state)))     
      ((and  (eq? (car condition) '!) (list? (cdr condition))) (cons [(not (M_boolean(cdr condition) state))] [(M_boolean(cdr (cdr condition)) state)]))
     
      ((eq? (car condition) '<) (< (M_value(cdr condition) state) (M_value(cdr(cdr condition)) state)))
      ((eq? (car condition) '>) (> (M_value(cdr condition) state) (M_value(cdr(cdr condition)) state)))
      ((eq? (car condition) '=) (= (M_value(cdr condition) state) (M_value(cdr(cdr condition)) state))))))


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
    
(define M_state
  (lambda (stmt state)
    (cond
      [(eq? (operator stmt) 'var) state]
      [(eq? (operator stmt) '=) state]
      [(eq? (operator stmt) 'if) state]
      [(eq? (operator stmt) 'while) state]
      [else (error 'stmt-not-defined)])))

(define remove-cps
  (lambda (var state return)
    (cond
     [(null? (name-list state)) (return state)]
     [(eq? (car (name-list state)) var) (return (cdr-state state))]
     [else (remove-cps var (cdr-state state)
                       (lambda (v) (cons (cons (car (name-list state)) (name-list v)) (list (cons (car (val-list state)) (val-list v))))))])))

(define remove
  (lambda (var state)
    (remove-cps var state (lambda (v) v))))




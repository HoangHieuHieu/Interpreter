#lang racket
(require "simpleParser.rkt")

;; abstractions 

;; operator: Extracts the operator from the expression
(define operator
  (lambda (expression) (car expression)))

;; leftoperand: Returns the left operand of given statement
(define leftoperand cadr)

;; rightoperand: returns the right operand of given statement
(define rightoperand caddr)

;; condition: return the conditional statement
(define condition cadr)

;; while-body: returns the body of while loop
(define while-body caddr)

;; if-body1: returns the first statement of the if statement
(define if-body1 caddr)

;; if-body2: returns the second statement of the if statement
(define if-body2 cdddr)

;; name-list: returns the list of variable names
(define name-list car)

;; val-list: returns the list of variable values
(define val-list cadr)

;; first_stmt: returns the first part of the statements
(define first_stmt car)

;; remaining_stmts: returns the rest of the statements
(define remaining_stmts cdr)

;; new-frame: initializes new frame for the state
(define new-frame '(()()))

;; prev-frame: get the state outside of the block
(define prev-frame caddr)

;;add-frame: add new frame to the state
(define new-frame
  (lambda (state)
    (cons (new-frame) state )))

;; remove-frame: remove frame from the state
(define remove-frame 
  (lambda (state)
    (cdr state)))


;;init-state: initializes the state for the program
(define init-state '(()()()))

;; default break: send error message
(define breakOutsideLoopError
  (lambda (env) (error 'invalid-break)))

;;default continue: send error message
(define continueOutsideLoopError
  (lambda (env) (error 'invalid-continue)))

;; cdr-state: take a state return that state without its first binding 
(define cdr-state
  (lambda (state)
    (list (cdr (name-list state)) (cdr (val-list state)) (prev-frame state))))

;; declared?: check if a var is declared
(define declared?
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(null? (name-list state)) (declared? var (prev-frame state))]
      [(eq? (car (name-list state)) var) #t]
      [else (declared? var (cdr-state state))])))


;; M_boolean: returns the boolean value for given condition
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
      ((eq? (operator condition) '<=)   (<= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '>=)   (>= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '==)   (= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state)))
      ((eq? (operator condition) '!=)   (not (= (M_value (leftoperand condition) state) (M_value (rightoperand condition) state))))
      (else (error 'bad-boolean)))))

;; add: returns the state after adding new variable and its value
(define add
  (lambda (var value state)
    (cond
      ((or (null? (name-list state)) (not (declared? var state)))   (cons (cons var (name-list state)) (list (cons value (val-list state)) (prev-frame state))))
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

;; getVar: takes in a variable and returns its value 
(define getVar
  (lambda (var state)
    (cond
      ((null? state) (error 'undeclared-variable))
      ((null? (name-list state)) (getVar var (prev-frame state)))
      ((and (eq? var (car (name-list state))) (eq? (car (val-list state)) 'null)) (error 'variables-were-not-assigned-values))
      ((eq? var (car (name-list state))) (car (val-list state)))
      (else (getVar var (cdr-state state))))))


; M_state: take a statement and a state, return the state after execute the statement on the state  
(define M_state
  (lambda (stmt state break return continue)
    (cond
      [(null? stmt) state]
      [(pair? (operator stmt)) (M_state (remaining_stmts stmt) (M_state (first_stmt stmt) state break return continue) break return continue)]
      [(eq? (operator stmt) 'var) (declare stmt state)]
      [(eq? (operator stmt) '=) (assign stmt state)]
      [(eq? (operator stmt) 'if) (if-stmt stmt state break return continue)]
      [(eq? (operator stmt) 'while) (while-stmt-break stmt state return)]
      [(eq? (operator stmt) 'return) (return-stmt stmt return state)]
      [(eq? (operator stmt) 'break) (break state)]
      [(eq? (operator stmt) 'begin) (block (cdr stmt) state break return continue)]
      [(eq? (operator stmt) 'continue) (continue state)]
      [else (error 'stmt-not-defined)])))

;function to modify state
(define modify-state
  (lambda (var val state)
    (call/cc
     (lambda (break)
       (cond
         ((null? state) '())
         ((null? (name-list state)) (list (name-list state) (val-list state) (modify-state var val (prev-frame state))))
         ((eq? (car (name-list state)) var) (list (name-list state) (cons val (cdr (val-list state))) (prev-frame state)))
         (else (add (car (name-list state)) (car (val-list state)) (modify-state var val (cdr-state state)))))))))

;; return: return a value
;; return-stmt: return a value
(define return-stmt
  (lambda (stmt return state)
    (return (format-out (M_value (cadr stmt) state)))))

;; assign: binding a value to a variable
(define assign
  (lambda (stmt state)
    (if (or (declared? (leftoperand stmt) state) (eq? (operator stmt) 'var))
    (modify-state (leftoperand stmt) (M_value (rightoperand stmt) state) state)
    (error 'undeclared-variables))))
    
;; remove-cps: helper function for remove
(define remove-cps
  (lambda (var state return)
    (cond
     [(null? (name-list state)) (return state)]
     [(eq? (car (name-list state)) var) (return (cdr-state state))]
     [else (remove-cps var (cdr-state state)
                       (lambda (v) (return (list (cons (car (name-list state)) (name-list v)) (cons (car (val-list state)) (val-list v)) (prev-frame state)))))])))

;; remove a var binding out of the state, return the state after the removal
(define remove
  (lambda (var state)
    (remove-cps var state (lambda (v) v))))

;; declare: add a var binding into a state
(define declare
  (lambda (stmt state)
    (cond
      [(declared? (leftoperand stmt) state) (error 'used-variable)]
      [(null? (cddr stmt)) (add (leftoperand stmt) 'null state)]
      [else (add (leftoperand stmt) (M_value (rightoperand stmt) state) state)])))

;; while-stmt: perform a while statement
(define while-stmt
  (lambda(stmt state)
    (if (M_boolean (condition stmt) state)
      (while-stmt stmt (M_state (while-body stmt) state))
      state)))

(define while-stmt-break
  (lambda (stmt state return)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M_boolean condition state)
                            (loop condition body (M_state body state break return (lambda (v) (break (loop condition body v)))))
                                                          state))))
         (loop (condition stmt) (while-body stmt) state))))))

                             
; if: perform an if statement
;; if-stmt: perform an if statement
(define if-stmt
  (lambda (stmt state break return continue)
    (cond
      ((M_boolean (condition stmt) state) (M_state (if-body1 stmt) state break return continue))
      ((not (null? (if-body2 stmt))) (M_state (if-body2 stmt) state break return continue))
      (else state))))

;; M_value: takes an expression, return the value of it (either a boolean or an integer)
(define M_value
  (lambda (expression state)
    (cond
      [(eq? expression 'true) #t]
      [(eq? expression 'false)#f]
      [(number? expression) expression]
      [(not (list? expression)) (getVar expression state)]
      [(or (eq? (operator expression) '+) (eq? (operator expression) '-) (eq? (operator expression) '*) (eq? (operator expression) '/) (eq? (operator expression) '%))
       (M_integer expression state)]
      [else (M_boolean expression state)])))

;; interpret: Take in a file name and interpret the code in the file
(define interpret
     (lambda (filename)
       (call/cc
        (lambda (return)
          (M_state (parser filename) init-state breakOutsideLoopError return continueOutsideLoopError)))))


;; format-out: return format of the result
(define format-out
  (lambda (out)
    (cond
      [(number? out) out]
      [(eq? out #t) 'true]
      [else  'false])))

(define block
  (lambda (stmt state break return continue)
    (prev-frame (M_state stmt (append new-frame (list state)) (lambda (v) (break (prev-frame v))) return continue))))


;; throw
(define throw-stmt
  (lambda (stmt state throw)
    (throw (M_value (leftoperand stmt) state) state)))


;;throw-catch-try

(define throw-catch-try
  (lambda (catch-stmt state return break continue throw jump finally-block)
    (cond 
      ((null? catch-stmt)  (lambda (exception state) (throw exception (block finally-block  state return break continue throw))))
      ((not (eq? 'catch (car catch-stmt)))           (error 'Invalid catch statement))
      (else
       (lambda (exception state)
         (jump (block finally-block
                      (remove-frame (throw-catch-statement-list (rightoperand catch-stmt)
                                                                (add (car (leftoperand catch-stmt)) exception (add-frame state))
                                                                return 
                                                                (lambda (state2) (break (add-frame state2))) 
                                                                (lambda (state2) (continue (add-frame state2))) 
                                                                (lambda (v state2) (throw v (add-frame state2)))))
                                                                return break continue throw)))))))
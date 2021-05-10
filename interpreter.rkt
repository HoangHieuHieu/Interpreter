#lang racket
;(require "simpleParser.rkt")
;(require "functionParser.rkt")
(require "classParser.rkt")
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

;; first-stmt: returns the first part of the statements
(define first-stmt car)

;; remaining-stmts: returns the rest of the statements
(define remaining-stmts cdr)

;; new-frame: initializes new frame for the state
(define new-frame '(()()))

;; add-frame: add new frame on to the current state
(define add-frame
  (lambda (state)
    (append new-frame (list state))))

;; prev-frame: get the state outside of the block
(define prev-frame caddr)

;;init-state: initializes the state for the program
(define init-state '(()()()))

;; get the try block
(define get-try-block cadr)

;; get the finally block
(define get-finally-block cadddr)

;;get formal params list
(define get-func-formal-params caddr)

;;get function body
(define get-func-body cadddr)

;;get function name
(define get-func-name cadr)

;; default break: send break error message
(define breakOutsideLoopError
  (lambda (env) (error 'invalid-break)))

;;default continue: send continue error message
(define continueOutsideLoopError
  (lambda (env) (error 'invalid-continue)))

;;default throw: send throw error message
(define uncaughtException
  (lambda (env v) (error 'invalid-throw)))


;; cdr-state: take a state return that state without its first binding 
(define cdr-state
  (lambda (state)
    (list (cdr (name-list state)) (cdr (val-list state)) (prev-frame state))))


;; interpret: Take in a file name and interpret the code in the file
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (format-out (static-main-exe 'main (car (get-main-class (class-interpreter (parser filename) init-state)))  (class-interpreter (parser filename) init-state)))))))

;; declared?: check if a var is declared
(define declared?
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(null? (name-list state)) (declared? var (prev-frame state))]
      [(eq? (car (name-list state)) var) #t]
      [else (declared? var (cdr-state state))])))

;; M-boolean: returns the boolean value for given condition
(define M-boolean
  (lambda (condition state throw compile-type this-object)
    (let* ((this-object-closure (getVar this-object state))
           (compile-closure (getVar compile-type state))
           (access-field-values cadr)
           (access-field-list cadr)
           (compile-closure-field-list (access-field-list compile-closure))
           (runtime-type (car this-object-closure))
           (runtime-closure (getVar runtime-type state)))
      (cond
        ((null? condition) #t)
        ((eq? condition 'true)   #t)
        ((eq? condition 'false)  #f)
        ((eq? (operator condition) 'funcall) (class-method-exe (cdr condition) state throw))
        ((not (list? condition)) (get-field (access-field-values this-object-closure) (field-position condition compile-closure-field-list)))
        ((eq? (operator condition) 'dot) (get-field (access-field-values (getVar (leftoperand condition) state)) (field-position (rightoperand condition) (access-field-list (getVar (leftoperand condition) state)))))
        ((boolean? (car condition))   (return-stmt (car condition) state compile-type this-object))
        ; boolean operation 
        ((eq? (operator condition) '&&)    (and (M-boolean(leftoperand condition) state throw compile-type this-object)   (M-boolean(rightoperand condition) state throw compile-type this-object)))          
        ((eq? (operator condition) '||)    (or (M-boolean(leftoperand condition) state throw compile-type this-object)    (M-boolean(rightoperand condition) state throw compile-type this-object)))     
        ((eq? (operator condition) '!)     (not (M-boolean (leftoperand condition) state throw compile-type this-object)))
        ; boolean function
        ((eq? (operator condition) 'funcall) (func-exe (cdr condition) state throw))
        ; comparision operator
        ((eq? (operator condition) '<)    (< (M-value (leftoperand condition) state throw compile-type this-object) (M-value (rightoperand condition) state throw compile-type this-object)))
        ((eq? (operator condition) '>)    (> (M-value (leftoperand condition) state throw compile-type this-object) (M-value (rightoperand condition) state throw compile-type this-object)))
        ((eq? (operator condition) '<=)   (<= (M-value (leftoperand condition) state throw compile-type this-object) (M-value (rightoperand condition) state throw compile-type this-object)))
        ((eq? (operator condition) '>=)   (>= (M-value (leftoperand condition) state throw compile-type this-object) (M-value (rightoperand condition) state throw compile-type this-object)))
        ((eq? (operator condition) '==)   (= (M-value (leftoperand condition) state throw compile-type this-object) (M-value (rightoperand condition) state throw compile-type this-object)))
        ((eq? (operator condition) '!=)   (not (= (M-value (leftoperand condition) state throw compile-type this-object) (M-value (rightoperand condition) state throw compile-type this-object))))
        (else (error 'bad-boolean))))))

;; add: returns the state after adding new variable and its value
(define add
  (lambda (var value state)
    (cons (cons var (name-list state)) (list (cons value (val-list state)) (prev-frame state)))))

;; denotational mapping of an expression to an integer
(define M-integer
  (lambda (expression state throw compile-type this-object)
    (let* ((this-object-closure (getVar this-object state))
           (compile-closure (getVar compile-type state))
           (access-field-values cadr)
           (access-field-list cadr)
           (compile-closure-field-list (access-field-list compile-closure))
           (runtime-type (car this-object-closure))
           (runtime-closure (getVar runtime-type state)))
      (cond
        ((null? expression) 0)
        ((number? expression) expression)
        ((eq? (operator expression) 'funcall) (class-method-exe (cdr expression) state throw))
        ((not (list? expression)) (get-field (access-field-values this-object-closure) (field-position expression compile-closure-field-list)))
        ((eq? (operator expression) 'dot) (get-field (access-field-values (getVar (leftoperand expression) state)) (field-position (rightoperand expression) (access-field-list (getVar (leftoperand expression) state)))))
        ((and (null? (cddr expression)) (eq? (operator expression) '+)) (+ 0 (M-value (leftoperand expression) state throw compile-type this-object)))
        ((and (null? (cddr expression)) (eq? (operator expression) '-)) (- 0 (M-value (leftoperand expression) state throw compile-type this-object)))
        ((eq? (operator expression) '+) (+ (M-value (leftoperand expression) state throw compile-type this-object) (M-value (rightoperand expression) state throw compile-type this-object)))
        ((eq? (operator expression) '-) (- (M-value (leftoperand expression) state throw compile-type this-object) (M-value (rightoperand expression) state throw compile-type this-object)))
        ((eq? (operator expression) '*) (* (M-value (leftoperand expression) state throw compile-type this-object) (M-value (rightoperand expression) state throw compile-type this-object)))
        ((eq? (operator expression) '/) (quotient (M-value (leftoperand expression) state throw compile-type this-object) (M-value (rightoperand expression) state throw compile-type this-object)))
        ((eq? (operator expression) '%) (remainder (M-value (leftoperand expression) state throw compile-type this-object) (M-value (rightoperand expression) state throw compile-type this-object)))
        (else (error 'bad-operator))))))

;; getVar: takes in a variable and returns its value 
(define getVar
  (lambda (var state)
    (cond
      ((null? state) (error 'undeclared-variable))
      ((null? (name-list state)) (getVar var (prev-frame state)))
      ((and (eq? var (car (name-list state))) (eq? (car (val-list state)) 'null)) (error 'variables-were-not-assigned-values))
      ((eq? var (car (name-list state))) (car (val-list state)))
      (else (getVar var (cdr-state state))))))

;;create function closure
(define func-closure
  (lambda (stmt)
    (list (get-func-body stmt) (get-func-formal-params stmt) (lambda (state) (get-func-state (get-func-name stmt) state)))))

(define class-method-closure
  (lambda (stmt classname)
    (list (get-func-body stmt) (get-func-formal-params stmt) (lambda (state) (java-func-state state classname '() '()))  classname)))

(define java-func-state
  (lambda (state class-name l1 l2)
    (cond
      [(null? state) state]
      [(null? (name-list state)) (java-func-state (prev-frame state) class-name '() '())]
      [(eq? (car (name-list state)) class-name) (list (append l1 (name-list state)) (append l2 (val-list state)) (prev-frame state))]
      [else (java-func-state (cdr-state state) class-name (cons (car (name-list state)) l1) (cons (car (val-list state)) l2))])))

;; get the state from the function closure
(define get-func-state
  (lambda (f-name state)
    (cond
      ((null? state) state)
      ((null? (name-list state)) (get-func-state f-name (prev-frame state)))
      ((eq? (car (name-list state)) f-name) state)
      (else (get-func-state f-name (cdr-state state))))))

;; bind the value of actual parameter to the formal parameter into the function state
(define create-bindings
  (lambda (formal-params actual-params cur-state new-state throw compile-type this-object)
    (cond
      ((or (and (null? formal-params) (not (null? actual-params))) (and (not (null? formal-params)) (null? actual-params))) (error 'Mismatched-parameters-and-arguments))
      ((null? formal-params) new-state)
      (else (create-bindings (cdr formal-params)
                             (cdr actual-params)
                             cur-state
                             ;(add (car formal-params) (M-value (car actual-params) cur-state throw compile-type this-object) (M-state-exp (car actual-params) new-state throw))
                             (add (car formal-params) (M-value (car actual-params) cur-state throw compile-type this-object) new-state)
                             throw
                             compile-type
                             this-object)))))

;; execute the function in main
(define func-exe
  (lambda (funcall state throw)
    (call/cc
     (lambda (return)
       (let* ((closure (getVar (car funcall) state))
              (body (car closure))
              (formal-params (cadr closure))
              (actual-params (cdr funcall))
              (f-state-1 ((caddr closure) state))
              (f-state-2 (create-bindings formal-params actual-params state (add-frame f-state-1) throw)))
         (M-state body f-state-2 breakOutsideLoopError (lambda (v) (return (car v))) continueOutsideLoopError (lambda (v func-env) (throw v (update-state state (prev-frame func-env))))))))))
              
; M-state: take a statement and a state, return the state after execute the statement on the state  
(define M-state
  (lambda (stmt state break return continue throw compile-type this-object)
    (cond
      [(null? stmt) state]
      [(pair? (operator stmt)) (M-state (remaining-stmts stmt) (M-state (first-stmt stmt) state break return continue throw compile-type this-object) break return continue throw  compile-type this-object)]
      [(eq? (operator stmt) 'function)  (add (get-func-name stmt) (func-closure stmt) state)]
      [(eq? (operator stmt) 'funcall) (M-state-java-funcall-result-env stmt state throw this-object)]
      [(eq? (operator stmt) 'var) (declare stmt state throw compile-type this-object)]
      [(eq? (operator stmt) '=) (assign stmt state throw compile-type this-object)]
      [(eq? (operator stmt) 'if) (if-stmt stmt state break return continue throw compile-type this-object)]
      [(eq? (operator stmt) 'while) (while-stmt stmt state return throw compile-type this-object)]
      [(eq? (operator stmt) 'return) (return-stmt stmt return state throw compile-type this-object)]
      [(eq? (operator stmt) 'break) (break state)]
      [(eq? (operator stmt) 'begin) (block (cdr stmt) state break return continue throw compile-type this-object)]
      [(eq? (operator stmt) 'continue) (continue state)]
      [(eq? (operator stmt) 'try) (try-stmt stmt state break return continue throw compile-type this-object)]
      [(eq? (operator stmt) 'throw) (throw-stmt stmt state throw compile-type this-object)]
      [else (error 'stmt-not-defined)])))

;; parsing global variable and function declaration to the global state
(define M-state-func
  (lambda (stmt state throw compile-type this-object)
    (cond
      [(null? stmt) state]
      [(pair? (operator stmt)) (M-state-func (remaining-stmts stmt) (M-state-func (first-stmt stmt) state throw) throw)]
      [(eq? (operator stmt) 'var) (declare stmt state throw compile-type this-object)]
      [(eq? (operator stmt) 'function) (add (get-func-name stmt) (func-closure stmt) state)]
      [else (error 'invalid-stmt)])))

;; modify-state: take in a variable, a value, and a state. Return a state after binding the value to the variable.
(define modify-state
  (lambda (var val state)
    (call/cc
     (lambda (break)
       (cond
         ((null? state) '())
         ((null? (name-list state)) (list (name-list state) (val-list state) (modify-state var val (prev-frame state))))
         ((eq? (car (name-list state)) var) (list (name-list state) (cons val (cdr (val-list state))) (prev-frame state)))
         (else (add (car (name-list state)) (car (val-list state)) (modify-state var val (cdr-state state)))))))))


;; return-stmt: take in a return statement and return the value after calling the return continuation
(define return-stmt
  (lambda (stmt return state throw compile-type this-object)
    (return (list (M-value (cadr stmt) state throw compile-type this-object) state))))

;; return the state of side-effect
(define M-state-exp
  (lambda (exp state throw compile-type this-object)
    (cond
      [(not (pair? exp)) state]
      [(eq? (operator exp) '=) (assign exp state throw compile-type this-object)]
      ((eq? (operator exp) 'funcall) (M-state-java-funcall-result-env (cdr exp) state throw this-object))
      (else state))))

;; update the state according to the function environment
(define update-state
  (lambda (state closure)
    (cond
      [(null? closure) state]
      [(null? (name-list closure)) (update-state state (prev-frame closure))]
      [else (modify-state (car (name-list closure)) (car (val-list closure)) (update-state state (cdr-state closure)))])))

;; return the state after executing a function call
(define M-state-funcall-result-env
  (lambda (funcall state throw)
    (call/cc
     (lambda (return)
       (let* ((closure (getVar (car funcall) state))
              (body (car closure))
              (formal-params (cadr closure))
              (actual-params (cdr funcall))
              (f-state-1 ((caddr closure) state))
              (f-state-2 (create-bindings formal-params actual-params state (add-frame f-state-1) throw)))
         (update-state state (M-state body f-state-2 breakOutsideLoopError (lambda (v) (return (update-state state (prev-frame (cadr v))))) continueOutsideLoopError throw)))))))

(define modify-field-list
  (lambda (field-list position value)
    (cond
      [(null? field-list) (error 'invalid-field)]
      [(= position 0) (cons value (cdr field-list))]
      [else (cons (car field-list) (modify-field-list (cdr field-list) (- position 1) value))])))
;; assign: binding a value to a variable
(define assign
  (lambda (stmt state throw compile-type this-object)
    ;(if (or (declared? (leftoperand stmt) state) (eq? (operator stmt) 'var))
    ;(modify-state (leftoperand stmt) (M-value (rightoperand stmt) state throw compile-type this-object) (M-state-exp (rightoperand stmt) state throw))))
    (let ((this-object-closure (getVar this-object state))
          (access-field-val cadr)
          (access-field-list cadr)
          (run-type car)
          (compile-type-closure (getVar compile-type state))
          )
      (cond
        [(and (list? (leftoperand stmt)) (eq? (operator (leftoperand stmt)) 'dot))
         (let* ((object-name (leftoperand (leftoperand stmt)))
               (object-closure (getVar object-name state))
               (object-class-closure (getVar (run-type object-closure) state))
               (field-name (rightoperand (leftoperand stmt))))
           (modify-state object-name
                        (list (run-type object-closure)(modify-field-list (access-field-val object-closure) (field-position field-name (access-field-list object-class-closure)) (M-value (rightoperand stmt) state throw compile-type this-object)))
                        state))]
        [(declared? (leftoperand stmt) state) (modify-state (leftoperand stmt) (M-value (rightoperand stmt) state throw compile-type this-object) state)]
          [else (modify-state this-object
                        (list (run-type this-object-closure)(modify-field-list (access-field-val this-object-closure) (field-position (leftoperand stmt) (access-field-list compile-type-closure)) (M-value (rightoperand stmt) state throw compile-type this-object)))
                        state)]))))
  
;(error 'undeclared-variables))))
    
; remove-cps: helper function for remove
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
  (lambda (stmt state throw compile-type this-object)
    (cond
      [(null? (cddr stmt)) (add (leftoperand stmt) 'null state)]
      ;[else (add (leftoperand stmt) (M-value (rightoperand stmt) state throw compile-type this-object) (M-state-exp (rightoperand stmt) state throw))])))
      [else (add (leftoperand stmt) (M-value (rightoperand stmt) state throw compile-type this-object) state)])))
;; while-stmt: perform a while statement with break and continue
(define while-stmt
  (lambda (stmt state return throw compile-type this-object)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M-boolean condition state throw compile-type this-object)
                            (loop condition body (M-state body state break return (lambda (v) (break (loop condition body v))) throw compile-type this-object))
                            state))))
         (loop (condition stmt) (while-body stmt) state))))))
                             
;; if-stmt: perform an if statement
(define if-stmt
  (lambda (stmt state break return continue throw compile-type this-object)
    (cond
      ((M-boolean (condition stmt) state throw compile-type this-object) (M-state (if-body1 stmt) state break return continue throw compile-type this-object) compile-type this-object)
      ((not (null? (if-body2 stmt))) (M-state (if-body2 stmt) state break return continue throw compile-type this-object))
      (else state))))

(define get-field
  (lambda (field-list position)
    (cond
      ((null? field-list) (error 'invalid-field))
      ((eq? position 0) (car field-list))
      (else (get-field (cdr field-list) (- position 1))))))

(define field-position
  (lambda (name name-list)
    (letrec ((reversed-list (reverse name-list))
             (position (lambda (name lis acc)
                         (cond
                           [(null? lis) (error 'invalid-field)]
                           [(eq? (car lis) name) acc]
                           [else  (position name (cdr lis) (+ acc 1))]))))
      (position name reversed-list 0))))

;; M-value: takes an expression, return the value of it (either a boolean or an integer)
(define M-value
  (lambda (expression state throw compile-type this-object)
    (let* ((this-object-closure (getVar this-object state))
           (compile-closure (getVar compile-type state))
           (access-field-values cadr)
           (access-field-list cadr)
           (compile-closure-field-list (access-field-list compile-closure))
           (runtime-type (car this-object-closure))
           (runtime-closure (getVar runtime-type state)))
      (cond
        [(eq? expression 'true) #t]
        [(eq? expression 'false)#f]
        [(number? expression) expression]
        [(and (not (list? expression)) (declared? expression state)) (getVar expression state)]
        [(not (list? expression)) (get-field (access-field-values this-object-closure) (field-position expression compile-closure-field-list))]
        [(eq? (operator expression) 'new) (create-object-closure expression state)]
        [(eq? (operator expression) 'funcall) (class-method-exe expression state throw this-object)]
        [(eq? (operator expression) 'dot)
         (let ((object-closure
                (cond
                  [(and (list? (leftoperand expression)) (eq? (operator (leftoperand expression)) 'new))(create-object-closure (leftoperand expression) state)]
                  [(or (eq? (leftoperand expression) 'this) (eq? (leftoperand expression) 'super)) (getVar 'this state)]
                  [else  (getVar (leftoperand expression) state)]))
               (object-class-closure
                (if (eq? (leftoperand expression) 'super)
                    (getVar (get-super-class compile-closure) state)
                    compile-closure)))
         (get-field (access-field-values object-closure) (field-position (rightoperand expression) (access-field-list object-class-closure))))]
        ;[(eq? (operator expression) '=) (getVar (leftoperand expression) (M-state-exp (leftoperand expression) state throw ))] 
        [(or (eq? (operator expression) '+) (eq? (operator expression) '-) (eq? (operator expression) '*) (eq? (operator expression) '/) (eq? (operator expression) '%))
         (M-integer expression state throw compile-type this-object)]
        [else (M-boolean expression state throw compile-type this-object)]))))

;; format-out: return format of the result
(define format-out
  (lambda (out)
    (cond
      [(number? out) out]
      [(eq? out #t) 'true]
      [else  'false])))

;; return the catch statement after removing "catch"
(define get-catch-stmt caddr)

;; create the try block in the format that can be interpreted by the code after removing the "try"
(define make-try-block
  (lambda (stmt)
    (cons 'begin (get-try-block stmt))))

;; return the catch block in the format that can be interpreted by the code
(define make-catch-block
  (lambda (stmt)
    (get-catch-stmt stmt)))

;; return the exception variable
(define exception-name caadr)

;; block: take in a block of statement, return the state after interpret the block of statements
(define block
  (lambda (stmt state break return continue throw compile-type this-object)
    (prev-frame (M-state stmt (append new-frame (list state)) (lambda (v) (break (prev-frame v))) return (lambda (v) (continue (prev-frame v))) (lambda (ex v) (throw ex (prev-frame v))) compile-type this-object))))

;; create the finally block in the format that can be interpreted by the code
(define make-finally-block
  (lambda (stmt)
    (if (null? (get-finally-block stmt))
        '()
        (cons 'begin (cadr (get-finally-block stmt))))))

;; create the continuation for the throw statement. It runs when the throw statement is called. If there is no catch, return the current state. Otherwise, interpret the catch block and return the state after the catch block
(define throw-catch-continuation
  (lambda (catch-stmt exception state break return continue throw catch-break finally-block compile-type this-object)
    (cond
      ((null? catch-stmt) (M-state finally-block state break return continue throw compile-type this-object))
      ((not (eq? (operator catch-stmt) 'catch)) (error 'invalid-catch-statement))
      (else (catch-break (M-state finally-block (prev-frame (M-state (make-catch-block catch-stmt) (add (exception-name catch-stmt) exception (append new-frame (list state)))
                                                                     (lambda (v1) (break (prev-frame v1)))
                                                                     return
                                                                     (lambda (v1) (continue (prev-frame v1)))
                                                                     (lambda (ex v2) (throw ex (prev-frame v2))) compile-type this-object)) break return continue throw compile-type this-object))))))

;; try-catch statement: take in the try-catch block and finally statement. Interpret the try block with new break, continue and throw. Interpreting the finally block with the state returned after interpreting the try block. 
(define try-stmt
  (lambda (stmt state break return continue throw compile-type this-object)
    (call/cc
     (lambda (catch-break)
       (let* ((finally-block (make-finally-block stmt))
              (try-block (make-try-block stmt))
              (new-break (lambda (v) (break (M-state finally-block v break return continue throw compile-type this-object))))
              (new-continue (lambda (v) (continue (M-state finally-block v break return continue throw compile-type this-object))))
              (new-return (lambda (v) (begin (M-state finally-block state break return continue throw compile-type this-object) (return v))))
              (new-throw (lambda (ex v)  (throw-catch-continuation (get-catch-stmt stmt) ex v break return continue throw catch-break finally-block))))
         (M-state finally-block
                  (M-state try-block state new-break new-return new-continue new-throw compile-type this-object)
                  break return continue throw compile-type this-object))))))

;; throw-stmt: take in the throw statement and call the throw continuation.
(define throw-stmt
  (lambda (stmt state throw compile-type this-object)
    (throw (M-value (cadr stmt) state throw) state compile-type this-object)))
(define class-name cadr)
(define super-class caddr)
(define class-body cadddr)
(define super-class-name cadr)

(define class-closure
  (lambda (stmt state)
    (let* ((super (if (null? (super-class stmt))
                     'null
                     (super-class-name (super-class stmt))))
          (super-closure (if (null? (super-class stmt))
                     '(null ()()()())
                     (getVar super state)))
          (body (class-body stmt)))
      (cons super (interpret-class-body body (cdr super-closure) (class-name stmt))))))

(define interpret-class-body
  (lambda (body closure classname)
    (let ((field-names car)
          (field-inits cadr)
          (method-name caddr)
          (method-closures cadddr))
      (cond
        [(null? body) closure]
        [(list? (operator body)) (interpret-class-body (car body) (interpret-class-body (cdr body) closure classname) classname)]
        [(eq? (operator body) 'var)
         (if (null? (cddr body))
             (list (cons (leftoperand body) (field-names closure)) (cons 'null (field-inits closure)) (method-name closure) (method-closures closure))
             (list (cons (leftoperand body) (field-names closure)) (cons (rightoperand body) (field-inits closure)) (method-name closure) (method-closures closure)))]
        [(eq? (operator body) 'function)(list (field-names closure) (field-inits closure) (cons (get-func-name body) (method-name closure)) (cons (class-method-closure body classname) (method-closures closure)))]
        [(eq? (operator body) 'static-function) (list (field-names closure) (field-inits closure) (cons (get-func-name body) (method-name closure)) (cons (class-method-closure body classname) (method-closures closure)))]))))

(define class-interpreter
  (lambda (stmt state)
    (cond
      [(null? stmt) state]
      [(list? (operator stmt)) (class-interpreter (cdr stmt) (class-interpreter (car stmt) state))]
      [(eq? (operator stmt) 'class) (add (class-name stmt) (class-closure stmt state) state)]
      ;[(eq? (operator stmt) 'var) (declare stmt state compile-type this-object)]
      [else (error 'invalid-stmt)])))

(define get-main-class
  (lambda (state)
    (cond
      ((null? state) (error 'no-main-class))
      ((null? (val-list state)) (get-main-class (prev-frame state)))
      ((have-main-func (get-func-list-from-closure (car (val-list state)))) (list (car (name-list state))(car (val-list state))))
      (else (get-main-class (cdr-state state))))))
       
(define get-func-list-from-closure cadddr)

(define get-init-field-value caddr)

(define have-main-func
  (lambda (func-list)
    (cond
      ((null? func-list) #f)
      ((eq? (car func-list) 'main) #t)
      (else (have-main-func (cdr func-list))))))

(define create-object-closure
  (lambda (stmt state)
    (let* ((ob-class-name (leftoperand stmt))
           (ob-class-closure (getVar ob-class-name state))
           (init-field-value (reverse (get-init-field-value ob-class-closure))))
      (list ob-class-name init-field-value))))

(define get-class-method-closure
  (lambda (func-list closure-list name)
    (cond
      ((or (null? func-list) (null? closure-list)) (error 'method-not-defined))
      ((eq? (car func-list) name) (car closure-list))
      (else (get-class-method-closure (cdr func-list) (cdr closure-list) name)))))

(define get-func-closures
  (lambda (closure)
    (car (cddddr closure))))

(define get-super-class car)
(define class-method-exe
  (lambda (funcall state throw this-object)
    (call/cc
     (lambda (return)
       (letrec ((method-full-name (leftoperand funcall))
                (this-ob-name (if (list? method-full-name)
                                  (cond
                                    ((eq? (operator method-full-name) 'dot) (leftoperand method-full-name))
                                    (else (error 'invalid-statement)))
                                  this-object))
                (method-name (if (list? method-full-name)
                                 (cond
                                   ((eq? (operator method-full-name) 'dot) (rightoperand method-full-name))
                                   (else (error 'invalid-statement)))
                                 method-full-name))
                (this-object-closure (cond
                                       [(and (list? this-ob-name) (eq? (operator this-ob-name) 'new)) (create-object-closure this-ob-name state)]
                                       [(eq? this-ob-name 'super) (getVar 'this state)]
                                       [else  (getVar this-ob-name state)]))
                (this-class-name (car this-object-closure))
                (this-class-closure (getVar this-class-name state))
                (method-closure (get-class-method-closure (get-func-list-from-closure this-class-closure) (get-func-closures this-class-closure) method-name))
                (compile-type (if (eq? this-ob-name 'super)
                                  (get-super-class (getVar (cadddr method-closure) state)) 
                                  (cadddr method-closure)))
                (body (car method-closure))
                (get-env (caddr method-closure))
                (f-state (get-env state))
                (formal-params (cadr method-closure))
                (actual-params (cddr funcall)) 
                (f-state-1 (create-bindings formal-params actual-params state (add-frame f-state) throw compile-type this-object))
                (f-state-2 (add 'this this-object-closure f-state-1)))
         (M-state body f-state-2 breakOutsideLoopError (lambda (v) (return (car v))) continueOutsideLoopError (lambda (v func-env) (throw v (java-update-state state (prev-frame func-env) this-ob-name))) compile-type 'this))))))

(define M-state-java-funcall-result-env
  (lambda (funcall state throw this-object)
    (call/cc
     (lambda (return)
       (letrec ((method-full-name (leftoperand funcall))
                (this-ob-name (if (list? method-full-name)
                                  (cond
                                    ((eq? (operator method-full-name) 'dot) (leftoperand method-full-name))
                                    (else (error 'invalid-statement)))
                                  this-object))
                (method-name (if (list? method-full-name)
                                 (cond
                                   ((eq? (operator method-full-name) 'dot) (rightoperand method-full-name))
                                   (else (error 'invalid-statement)))
                                 method-full-name))
                (this-object-closure (cond
                                       [(and (list? this-ob-name) (eq? (operator this-ob-name) 'new)) (create-object-closure this-ob-name state)]
                                       [(eq? this-ob-name 'super) (getVar 'this state)]
                                       [else  (getVar this-ob-name state)]))
                (this-class-name (car this-object-closure))
                (this-class-closure (getVar this-class-name state))
                (method-closure (get-class-method-closure (get-func-list-from-closure this-class-closure) (get-func-closures this-class-closure) method-name))
                (compile-type (if (eq? this-ob-name 'super)
                                  (get-super-class (getVar (cadddr method-closure) state)) 
                                  (cadddr method-closure)))
                (body (car method-closure))
                (get-env (caddr method-closure))
                (f-state (get-env state))
                (formal-params (cadr method-closure))
                (actual-params (cddr funcall)) 
                (f-state-1 (create-bindings formal-params actual-params state (add-frame f-state) throw compile-type this-object))
                (f-state-2 (add 'this this-object-closure f-state-1)))
         (java-update-state state
                            (M-state body f-state-2 breakOutsideLoopError (lambda (v) (return (car v))) continueOutsideLoopError (lambda (v func-env) (throw v (java-update-state state (prev-frame func-env) this-ob-name))) compile-type 'this)
                            this-ob-name))))))
           
(define java-update-state
  (lambda (state env this-object-name)
    (let ((object-closure (getVar 'this env)))
      (modify-state this-object-name object-closure state))))
      
(define static-main-exe
  (lambda (name class state)
    (call/cc
     (lambda (return)
       (letrec ((main-class-closure (getVar class state))
                (method-closure (get-class-method-closure (get-func-list-from-closure main-class-closure) (get-func-closures main-class-closure) name))
                (body (car method-closure))
                (get-env (caddr method-closure))
                (f-state (get-env state))
                (f-state-1 (add-frame (add 'main-class (create-object-closure (list 'new class) state) f-state))))
         (M-state body f-state-1 breakOutsideLoopError (lambda (v) (return (car v))) continueOutsideLoopError uncaughtException class 'main-class))))))

    
;(define M-state-funcall-result-env
;  (lambda (funcall state throw)
;    (call/cc
;     (lambda (return)
;       (let* ((closure (getVar (car funcall) state))
;              (body (car closure))
;              (formal-params (cadr closure))
;              (actual-params (cdr funcall))
;              (f-state-1 ((caddr closure) state))
;              (f-state-2 (create-bindings formal-params actual-params state (add-frame f-state-1) throw)))
;         (update-state state (M-state body f-state-2 breakOutsideLoopError (lambda (v) (return (update-state state (prev-frame (cadr v))))) continueOutsideLoopError throw)))))))
 


